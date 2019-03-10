-- | Call in from the command line
--
-- Here all the parts are wired together:
-- WAI, WebSockets, connection to the IRC network.
--
-- No MonadBunyan here - it's easiest to stay in IO
-- and call into the various other stacks.
module Thunderbuns.CmdLine where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Dhall (auto, input)
import Network.HTTP.Types (Status(..))
import Network.Wai (Application, Request(..), Response, responseStatus)
import Network.Wai.Application.Static
  ( StaticSettings(..)
  , defaultWebAppSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions)
import System.Log.Bunyan
import System.Log.Bunyan.Context (someException)
import System.Log.Bunyan.LogText
import System.Log.Bunyan.Types (textToPriority)
import qualified Thunderbuns.Config as C
import qualified Thunderbuns.Irc.Client as I
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Types as I
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Handler as W
import qualified Thunderbuns.WS.Types as W
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Async (Async, async, cancel, concurrently)
import UnliftIO.Exception (SomeException, bracket, finally)
import UnliftIO.MVar (newEmptyMVar, putMVar)
import UnliftIO.STM
import WaiAppStatic.Types (toPiece)

initialEnv :: IO C.Env
initialEnv = do
  envConfig <- input auto "./config.dhall"
  envLogQueue <- newEmptyMVar
  let pri = textToPriority (C.priority $ C.logging envConfig)
  _envLogger <- rootLogger "thunderbuns.root" pri (putMVar envLogQueue)
  envIrcConnection <- I.newConnection (C.server envConfig)
  envWSChan <- newBroadcastTChanIO
  pure
    C.Env
      { C.envConfig
      , C.envLogQueue
      , C._envLogger
      , C.envIrcConnection
      , C.envWSChan
      }

runMain :: IO ()
runMain = do
  env <- initialEnv
  let C.Env { C.envConfig
            , C.envLogQueue
            , C.envIrcConnection
            , C.envWSChan
            , C._envLogger
            } = env
  let conn = envIrcConnection
  withLogWriter envLogQueue (C.filename $ C.logging envConfig) $
    bracket
      (async $ runReaderT (I.runIrcClient (I.registerConnection conn) conn) env)
      (logAndCancel "Canceling IRC Connection" _envLogger)
      (const $
       void $
       concurrently
         (runWebServer (C.http envConfig) envIrcConnection envWSChan _envLogger)
         (atomically (dupTChan (I.fromServer conn)) >>=
          queueMessagesForWSClients _envLogger envWSChan))

queueMessagesForWSClients ::
     Logger -> TChan W.Response -> TChan I.Message -> IO ()
queueMessagesForWSClients lg dst src =
  forever
    (atomically $ do
       msg <- readTChan src
       writeTChan dst (W.classifyIrcMessage msg)) `finally`
  logDebug "queueMessagesForWSClients:  terminating" lg

logAndCancel :: MonadIO m => T.Text -> Logger -> Async a -> m ()
logAndCancel msg lg a = do
  logInfo msg lg
  cancel a

runWebServer ::
     MonadUnliftIO m
  => C.HttpConfig
  -> I.Connection
  -> TChan W.Response
  -> Logger
  -> m ()
runWebServer C.HttpConfig {C.port, C.staticDir} irccon responseChan =
  withNamedLogger "thunderbuns.http" id $ \lg -> do
    let dir = T.unpack staticDir :: FilePath
    logInfo (T.pack $ "Serving HTTP on port " <> show port) lg
    liftIO
      (runSettings
         (mySettings lg defaultSettings)
         (mainApplication dir irccon responseChan lg)) `finally`
      logInfo (T.pack $ "Stopping HTTP on port " <> show port) lg
  where
    mySettings :: Logger -> Settings -> Settings
    mySettings lg =
      setPort (fromIntegral port) >>>
      setGracefulShutdownTimeout (Just 5) >>> setOnException (onException lg)
      -- >>> setInstallShutdownHandler shutdownHandler
    -- posix stuff no windows
    -- shutdownHandler closeSocket =
    --  void $ installHandler sigTERM (CatchOnce $ closeSocket) Nothing
    onException :: Logger -> Maybe Request -> SomeException -> IO ()
    onException lg _ ex
      -- most of these are routine things
     = logRecord DEBUG (someException ex) "WARP exception handler" lg

mainApplication ::
     FilePath -> I.Connection -> TChan W.Response -> Logger -> Application
mainApplication fp irccon responseChan = logResponseTime serve
  where
    serve :: Logger -> Application
    serve lg =
      websocketsOr
        defaultConnectionOptions
        (wsApplication irccon responseChan lg)
        (staticApp settings)
    settings =
      (defaultWebAppSettings fp) {ssIndices = [fromJust $ toPiece "index.html"]}

wsApplication :: I.Connection -> TChan W.Response -> Logger -> ServerApp
wsApplication irccon responseChan lgX pending =
  flip (withNamedLogger "thunderbuns.ws" id) lgX $ \lg -> do
    gc <- acceptRequest pending >>= W.guardedConnection
    logInfo "Connection accepted, starting ping/irc subscription threads" lg
    bracket
      (startThreads gc lg)
      (stopThreads lg)
      (const $ runReaderT (W.handleConn irccon gc) lg)
  where
    startThreads :: W.GuardedConnection -> Logger -> IO (Async (), Async ())
    startThreads gc lg = do
      a1 <- async $ pingThread gc 0
      ch <- atomically $ dupTChan responseChan
      a2 <- async $ ircSubscription ch gc lg
      pure (a1, a2)
    stopThreads :: Logger -> (Async (), Async ()) -> IO ()
    stopThreads lg (a1, a2) = do
      logAndCancel "Stopping ping thread" lg a1
      logAndCancel "Stopping irc subscription thread" lg a2
    pingThread :: W.GuardedConnection -> Int -> IO ()
    pingThread gc !n = do
      threadDelay (10 * 1000 * 1000)
      W.sendGuardedPing gc ("ping " <> fromString (show n))
      pingThread gc (n + 1)
    ircSubscription :: TChan W.Response -> W.GuardedConnection -> Logger -> IO ()
    ircSubscription chan gc lg =
      forever $ do
        resp <- atomically $ readTChan chan
        runReaderT (W.sendResponse (I.server irccon) gc resp) lg

--
--
-- System.Log.Bunyan.Wai
--
--
logResponseTime :: (Logger -> Application) -> Logger -> Application
logResponseTime = go
  where
    go :: (Logger -> Application) -> Logger -> Application
    go app lg req respond =
      withNamedLogger
        "thunderbuns.http"
        (requestLoggingContext req)
        (logDuration' $ \cb lg' ->
           app lg' req $ \res -> do
             responded <- respond res
             cb (responseLoggingContext res M.empty)
             pure responded)
        lg

requestLoggingContext :: Request -> A.Object -> A.Object
requestLoggingContext req =
  M.insert
    "req"
    (A.Object
       (M.fromList
          [ ("remoteAddress", A.String (toText $ show $ remoteHost req))
          , ("method", A.String (toText $ requestMethod req))
          , ("url", A.String (toText $ rawPathInfo req))
          ]))

responseLoggingContext :: Response -> A.Object -> A.Object
responseLoggingContext res =
  M.insert
    "res"
    (A.Object $
     M.singleton "status" $
     A.Number $ fromIntegral $ statusCode $ responseStatus res)
