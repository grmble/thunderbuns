-- | Call in from the command line
--
-- Here all the parts are wired together:
-- WAI, WebSockets, connection to the IRC network.
--
-- No MonadBunyan here - it's easiest to stay in IO
-- and call into the various other stacks.
module Thunderbuns.CmdLine where

import Control.Concurrent (threadDelay)
import Control.Lens (set, view)
import Control.Monad.Reader (ask, runReaderT)
import qualified Data.Aeson as A
import Data.CaseInsensitive (original)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Dhall (auto, input)
import Network.HTTP.Types (Status(..))
import Network.Wai (Application, Request(..), Response, responseStatus)
import Network.Wai.Application.Static
  ( StaticSettings(..)
  , defaultWebAppSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets (isWebSocketsReq, websocketsOr)
import Network.Wai.Middleware.Approot (fromRequest, hardcoded)
import Network.Wai.Middleware.Gzip
  ( GzipFiles(..)
  , GzipSettings(..)
  , def
  , gzip
  , gzipFiles
  )
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.UrlMap (mapUrls, mount)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions)
import System.Log.Bunyan (withLogWriter)
import qualified System.Log.Bunyan as Bunyan
import System.Log.Bunyan.Context (someException)
import System.Log.Bunyan.LogText
import System.Log.Bunyan.RIO
import System.Log.Bunyan.Types (textToPriority)
import qualified Thunderbuns.Config as C
import Thunderbuns.Irc.Api
import qualified Thunderbuns.Irc.Config as I (ServerConfig(..))
import qualified Thunderbuns.Irc.Types as I (IrcConnection(..))
import Thunderbuns.Persist.Api (closeDatabase, openDatabase)
import Thunderbuns.Persist.Main (queueMessagesForWSClients)
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Api as W
import qualified Thunderbuns.WS.Main as W
import qualified Thunderbuns.WS.Types as W
import UnliftIO (timeout)
import UnliftIO.Async (Async, async, cancel, race)
import UnliftIO.Exception (SomeException, bracket, finally)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import UnliftIO.STM
import WaiAppStatic.Types (toPiece)

initialEnv :: C.Config -> MVar A.Object -> IO C.Env
initialEnv _envConfig _envLogQueue = do
  let pri = textToPriority (C.priority $ C.logging _envConfig)
  _envLogger <- rootLogger "thunderbuns.root" pri (putMVar _envLogQueue)
  _envIrcConnection <- runReaderT newConnection _envConfig
  _envWSChan <- newBroadcastTChanIO
  let dbc = view C.databaseConfig _envConfig
  _envDBPool <- runReaderT (openDatabase dbc) _envLogger
  pure
    C.Env
      { C._envConfig
      , C._envLogQueue
      , C._envLogger
      , C._envIrcConnection
      , C._envWSChan
      , C._envDBPool
      }

closeEnv :: C.Env -> IO ()
closeEnv env@C.Env {C._envDBPool} = runReaderT (closeDatabase _envDBPool) env

runMain :: IO ()
runMain = do
  cfg <- input auto "./config.dhall"
  lq <- newEmptyMVar
  withLogWriter lq (C.filename (C.logging cfg)) $
    bracket (initialEnv cfg lq) closeEnv $ \env -> do
      let wsChan = view C.envWSChan env
      flip runReaderT env $
        bracket
          (async superviseIrcClient)
          (logAndCancel "Canceling IRC Connection")
          (const $
           void $
           race
             (runWebServer wsChan)
             (dupMessageChan >>= flip queueMessagesForWSClients wsChan))

logAndCancel :: Bunyan r m => T.Text -> Async a -> m ()
logAndCancel msg a = do
  logInfo msg
  cancel a

runWebServer ::
     ( C.HasDatabasePool r
     , HasIrcConnection r
     , C.HasHttpConfig r
     , Bunyan r m
     , MonadUnliftIO m
     )
  => TChan W.Response
  -> m ()
runWebServer responseChan =
  withNamedLogger "thunderbuns.http" id $ do
    closeAction <- newEmptyMVar
    cfg <- view C.httpConfig
    let port = C.port cfg
    logInfo (T.pack $ "Serving HTTP on port " <> show port)
    env <- ask
    liftIO
      (runSettings
         (mySettings
            closeAction
            (fromIntegral port)
            (view logger env)
            defaultSettings)
         (mainApplication responseChan env)) `finally` do
      logInfo (T.pack $ "Stopping HTTP on port " <> show port)
      action <- timeout 50000 $ takeMVar closeAction
      maybe (pure ()) liftIO action
  where
    mySettings :: MVar (IO ()) -> Integer -> Logger -> Settings -> Settings
    mySettings closeAction port lg =
      setPort (fromIntegral port) >>>
      setGracefulShutdownTimeout (Just 0) >>>
      setOnException (onException lg) >>>
      setInstallShutdownHandler (putMVar closeAction)
    -- shutdownHandler closeSocket =
    --  void $ installHandler sigTERM (CatchOnce $ closeSocket) Nothing
    onException :: Logger -> Maybe Request -> SomeException -> IO ()
    onException lg _ ex
      -- most of these are routine things
     = Bunyan.logRecord DEBUG (someException ex) "WARP exception handler" lg

mainApplication ::
     forall r.
     (C.HasHttpConfig r, C.HasDatabasePool r, HasIrcConnection r, HasLogger r)
  => TChan W.Response
  -> r
  -> Application
mainApplication responseChan = logResponseTime serve
  where
    serve :: r -> Application
    serve env = do
      let srv = I.server $ view ircConnection env
      let cfg = view C.httpConfig env
      let settings =
            (defaultWebAppSettings (T.unpack $ C.staticDir cfg))
              {ssIndices = [fromJust $ toPiece "index.html"]}
      let authUser = I.nick srv
      let nickPass = I.nicksrvPassword srv
      let srvPass = I.serverPassword srv
      let setAppRootOrGuess =
            maybe fromRequest (hardcoded . T.encodeUtf8) (C.appRoot cfg)
      let root = normalizePrefix $ C.rootPrefix cfg
      basicAuth
        (\u p ->
           pure $
           toText u == authUser &&
           toText p == fromMaybe "" (nickPass <|> srvPass))
        "Thunderbuns" $
        gzip (def {gzipFiles = GzipPreCompressed GzipIgnore}) $
        setAppRootOrGuess $
        logWsRequestHeaders
          (websocketsOr
             defaultConnectionOptions
             (wsApplication responseChan env)
             (mapUrls (mount root (staticApp settings))))
          env
    normalizePrefix :: Text -> Text
    normalizePrefix s =
      let noHead =
            if "/" `T.isPrefixOf` s
              then T.tail s
              else s
          noTail =
            if "/" `T.isSuffixOf` noHead
              then T.dropEnd 1 noHead
              else noHead
       in noTail

wsApplication ::
     (C.HasDatabasePool r, HasIrcConnection r, HasLogger r)
  => TChan W.Response
  -> r
  -> ServerApp
wsApplication responseChan env pending =
  flip (Bunyan.withNamedLogger "thunderbuns.ws" id) (view logger env) $ \lg -> do
    gc <- acceptRequest pending >>= W.guardedConnection
    runReaderT
      (logInfo "Connection accepted, starting ping/irc subscription threads")
      lg
    bracket
      (startThreads gc lg)
      (stopThreads lg)
      (const $ runReaderT (W.handleConn gc) (set logger lg env))
  where
    startThreads :: W.GuardedConnection -> Logger -> IO (Async (), Async ())
    startThreads gc lg = do
      a1 <- async $ pingThread gc 0
      ch <- atomically $ dupTChan responseChan
      a2 <- async $ ircSubscription ch gc lg
      pure (a1, a2)
    stopThreads :: Logger -> (Async (), Async ()) -> IO ()
    stopThreads lg (a1, a2) =
      flip runReaderT lg $ do
        logAndCancel "Stopping ping thread" a1
        logAndCancel "Stopping irc subscription thread" a2
    pingThread :: W.GuardedConnection -> Int -> IO ()
    pingThread gc !n = do
      threadDelay (10 * 1000 * 1000)
      W.sendGuardedPing gc ("ping " <> fromString (show n))
      pingThread gc (n + 1)
    ircSubscription ::
         TChan W.Response -> W.GuardedConnection -> Logger -> IO ()
    ircSubscription chan gc lg =
      forever $ do
        resp <- atomically $ readTChan chan
        runReaderT (W.sendResponse gc resp) lg

--
--
-- System.Log.Bunyan.Wai
--
--
logResponseTime ::
     forall r. HasLogger r
  => (r -> Application)
  -> r
  -> Application
logResponseTime = go
  where
    go :: (r -> Application) -> r -> Application
    go app env req respond =
      Bunyan.withNamedLogger
        "thunderbuns.http"
        (requestLoggingContext req)
        (Bunyan.logDuration' DEBUG "http request" $ \cb lg -> do
           let env' = set logger lg env
           app env' req $ \res -> do
             responded <- respond res
             cb (responseLoggingContext res M.empty)
             pure responded)
        (view logger env)

logWsRequestHeaders :: HasLogger r => Application -> r -> Application
logWsRequestHeaders action env req respond = do
  when (isWebSocketsReq req) $
    Bunyan.logRecord
      DEBUG
      (requestHeaderContext req)
      "websocket request headers"
      (view logger env)
  action req respond

requestHeaderContext :: Request -> A.Object -> A.Object
requestHeaderContext req = M.insert "headers" headersObject
  where
    headersObject = A.Object (M.fromList headerList)
    headerList =
      fmap
        (\(k, v) -> (toText $ original k, A.String $ toText v))
        (requestHeaders req)

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
