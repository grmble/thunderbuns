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
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time.Clock.System (getSystemTime)
import Dhall (auto, input)
import Network.HTTP.Types (Status(..))
import Network.Wai (Application, Request(..), Response, responseStatus)
import Network.Wai.Application.Static
  ( StaticSettings(..)
  , defaultWebAppSettings
  , staticApp
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions)
import System.IO
import System.Log.Bunyan
import System.Log.Bunyan.LogText
import System.Log.Bunyan.Types (textToPriority)
import qualified Thunderbuns.Config as C
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Types as I
import Thunderbuns.Tlude
import Thunderbuns.WS.Handler
  ( GuardedConnection
  , guardedConnection
  , handleConn
  , sendGuardedPing
  , sendResponse
  )
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Async (Async, async, cancel)
import UnliftIO.Exception (IOException, bracket, catchIO, finally)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import UnliftIO.STM (TChan, atomically, dupTChan, readTChan)
import WaiAppStatic.Types (toPiece)

initialEnv :: IO C.Env
initialEnv = do
  envConfig <- input auto "./config.dhall"
  envLogQueue <- newEmptyMVar
  let pri = textToPriority (C.priority $ C.logging envConfig)
  _envLogger <- rootLogger "thunderbuns.root" pri (putMVar envLogQueue)
  envIrcConnection <- I.newConnection (C.server envConfig)
  pure C.Env {C.envConfig, C.envLogQueue, C._envLogger, C.envIrcConnection}

runMain :: IO ()
runMain = do
  env <- initialEnv
  let C.Env {C.envConfig, C.envLogQueue, C.envIrcConnection, C._envLogger} = env
  withLogWriter envLogQueue (C.filename $ C.logging envConfig) $
    bracket
      (async $ runReaderT (I.runIrcConnection envIrcConnection) env)
      (logAndCancel "Canceling IRC Connection" _envLogger)
      (const $ runWebServer (C.http envConfig) envIrcConnection _envLogger)

logAndCancel :: MonadIO m => T.Text -> Logger -> Async a -> m ()
logAndCancel msg lg a = do
  logInfo msg lg
  cancel a

runWebServer ::
     MonadUnliftIO m => C.HttpConfig -> I.Connection -> Logger -> m ()
runWebServer C.HttpConfig {C.port, C.staticDir} irccon =
  withNamedLogger "thunderbuns.http" id $ \lg -> do
    let p = fromIntegral port :: Int
    let dir = T.unpack staticDir :: FilePath
    logInfo (T.pack $ "Serving HTTP on port " <> show p) lg
    liftIO (run p (mainApplication dir irccon lg)) `finally`
      logInfo (T.pack $ "Stopping HTTP on port " <> show p) lg

mainApplication :: FilePath -> I.Connection -> Logger -> Application
mainApplication fp irccon = logResponseTime serve
  where
    serve :: Logger -> Application
    serve lg =
      websocketsOr
        defaultConnectionOptions
        (wsApplication irccon lg)
        (staticApp settings)
    settings =
      (defaultWebAppSettings fp) {ssIndices = [fromJust $ toPiece "index.html"]}

wsApplication :: I.Connection -> Logger -> ServerApp
wsApplication irccon lgX pending =
  flip (withNamedLogger "thunderbuns.ws" id) lgX $ \lg -> do
    gc <- acceptRequest pending >>= guardedConnection
    logInfo "Connection accepted, starting ping/irc subscription threads" lg
    bracket
      (startThreads gc lg)
      (stopThreads lg)
      (const $ runReaderT (handleConn irccon gc) lg)
  where
    startThreads :: GuardedConnection -> Logger -> IO (Async (), Async ())
    startThreads gc lg = do
      a1 <- async $ pingThread gc 0
      ch <- atomically $ dupTChan (I.fromServer irccon)
      a2 <- async $ ircSubscription ch gc lg
      pure (a1, a2)
    stopThreads :: Logger -> (Async (), Async ()) -> IO ()
    stopThreads lg (a1, a2) = do
      logAndCancel "Stopping ping thread" lg a1
      logAndCancel "Stopping irc subscription thread" lg a2
    pingThread :: GuardedConnection -> Int -> IO ()
    pingThread gc !n = do
      threadDelay (10 * 1000 * 1000)
      sendGuardedPing gc ("ping " <> fromString (show n))
      pingThread gc (n + 1)
    ircSubscription :: TChan I.Message -> GuardedConnection -> Logger -> IO ()
    ircSubscription chan gc lg =
      forever $ do
        msg <- atomically $ readTChan chan
        runReaderT (sendResponse (I.server irccon) gc msg) lg

-- XXX fix in bunyan (improve logDuration with a final modification of the context)
logResponseTime :: (Logger -> Application) -> Logger -> Application
logResponseTime app lg req respond = do
  start <- getSystemTime
  lg' <- namedLogger "thunderbuns.http" (requestLoggingContext req) lg
  app lg' req $ \res -> do
    responded <- respond res
    end <- getSystemTime
    let (fobj, msg) = duration start end
    let fobj' = responseLoggingContext res
    logRecord INFO (fobj' . fobj) msg lg'
    pure responded

{--
logResponseTime' :: (Logger -> Application) -> Logger -> Application
logResponseTime' app lg req respond =
  withNamedLogger
    "thunderbuns.http"
    (requestLoggingContext req)
    (thenLogDuration' callApp)
    lg
  where
    callApp lg =
      app lg req $ \res -> do
        responded <- respond res
        pure (responseLoggingContext res M.empty, responded)

thenLogDuration' :: MonadIO m => (Logger -> m (A.Object, a)) -> Logger -> m a
thenLogDuration' action lg = do
  start <- liftIO getSystemTime
  (ctx, a) <- action lg
  end <- liftIO getSystemTime
  uncurry (logRecord INFO) (duration start end) (modifyContext (M.union ctx) lg)
  pure a
--}

--
--
-- System.Log.Bunyan.Wai
--
--
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
