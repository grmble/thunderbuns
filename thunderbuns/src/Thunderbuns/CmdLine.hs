-- | Call in from the command line
--
-- Here all the parts are wired together:
-- WAI, WebSockets, connection to the IRC network.
--
-- No MonadBunyan here - it's easiest to stay in IO
-- and call into the various other stacks.
module Thunderbuns.CmdLine where

import Control.Concurrent (threadDelay)
import Control.Lens (view)
import Control.Monad.Reader (asks, runReaderT)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Persist.Sqlite (SqlBackend)
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
import Network.Wai.Middleware.Approot (fromRequest, hardcoded)
import Network.Wai.Middleware.Gzip (GzipSettings(..), GzipFiles(..), def, gzip, gzipFiles)
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
import qualified Thunderbuns.Irc.Client as I
import qualified Thunderbuns.Irc.Config as I
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Types as I
import Thunderbuns.Persist
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Handler as W
import qualified Thunderbuns.WS.Types as W
import UnliftIO (MonadUnliftIO, liftIO, timeout)
import UnliftIO.Async (Async, async, cancel, race)
import UnliftIO.Exception (SomeException, bracket, catch, finally)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import UnliftIO.STM
import WaiAppStatic.Types (toPiece)

initialEnv :: IO C.Env
initialEnv = do
  _envConfig <- input auto "./config.dhall"
  _envLogQueue <- newEmptyMVar
  let pri = textToPriority (C.priority $ C.logging _envConfig)
  _envLogger <- rootLogger "thunderbuns.root" pri (putMVar _envLogQueue)
  _envIrcConnection <- I.newConnection (C.server _envConfig)
  _envWSChan <- newBroadcastTChanIO
  let dbc = view C.databaseConfig _envConfig
  _envDBPool <-
    runCreateSqlitePool
      (C.connectionURL dbc)
      (fromIntegral $ C.poolSize dbc)
      (C.runMigrations dbc)
      _envLogger
  pure
    C.Env
      { C._envConfig
      , C._envLogQueue
      , C._envLogger
      , C._envIrcConnection
      , C._envWSChan
      , C._envDBPool
      }

runMain :: IO ()
runMain = do
  env <- initialEnv
  let conn = view C.envIrcConnection env
  let logQueue = view C.envLogQueue env
  let wsChan = view C.envWSChan env
  let ircConn = view C.envIrcConnection env
  withLogWriter logQueue (C.filename $ view C.logConfig env) $
    flip runReaderT env $
    bracket
      (async $ I.runIrcClient (I.registerConnection conn) conn)
      (logAndCancel "Canceling IRC Connection")
      (const $
       void $
       race
         (runWebServer (C._envDBPool env) ircConn wsChan)
         (atomically (dupTChan (I.fromServer conn)) >>=
          queueMessagesForWSClients (C._envDBPool env) wsChan))

queueMessagesForWSClients ::
     (Bunyan r m, MonadUnliftIO m)
  => Pool SqlBackend
  -> TChan W.Response
  -> TChan I.Message
  -> m ()
queueMessagesForWSClients pool dst src =
  withNamedLogger
    "thunderbuns.persist"
    id
    (go `catch` rego `finally`
     logDebug "queueMessagesForWSClients:  terminating")
  where
    rego e =
      logRecord INFO (someException e) "Caught exception, restarting" >> go
    go =
      forever $ do
        msg <- atomically $ readTChan src
        lg <- asks (view logger)
        response <- W.makeResponse msg
        liftIO $ runInsertResponse lg response pool
        atomically $ writeTChan dst response

logAndCancel :: Bunyan r m => T.Text -> Async a -> m ()
logAndCancel msg a = do
  logInfo msg
  cancel a

runWebServer ::
     (C.HasHttpConfig r, Bunyan r m, MonadUnliftIO m)
  => Pool SqlBackend
  -> I.Connection
  -> TChan W.Response
  -> m ()
runWebServer pool irccon responseChan =
  withNamedLogger "thunderbuns.http" id $ do
    closeAction <- newEmptyMVar
    cfg <- asks (view C.httpConfig)
    let port = C.port cfg
    logInfo (T.pack $ "Serving HTTP on port " <> show port)
    lg <- asks (view logger)
    liftIO
      (runSettings
         (mySettings closeAction (fromIntegral port) lg defaultSettings)
         (mainApplication cfg pool irccon responseChan lg)) `finally` do
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
     C.HttpConfig
  -> Pool SqlBackend
  -> I.Connection
  -> TChan W.Response
  -> Logger
  -> Application
mainApplication cfg pool irccon responseChan = logResponseTime serve
  where
    serve :: Logger -> Application
    serve lg =
      basicAuth
        (\u p ->
           pure $
           toText u == authUser &&
           toText p == fromMaybe "" (nickPass <|> srvPass))
        "Thunderbuns" $
      gzip (def {gzipFiles = GzipPreCompressed GzipIgnore}) $
      setAppRootOrGuess $
      mapUrls $
      mount (C.rootPrefix cfg) $
      websocketsOr
        defaultConnectionOptions
        (wsApplication pool irccon responseChan lg)
        (staticApp settings)
    settings =
      (defaultWebAppSettings (T.unpack $ C.staticDir cfg))
        {ssIndices = [fromJust $ toPiece "index.html"]}
    authUser = I.nick $ I.server irccon
    nickPass = I.nicksrvPassword $ I.server irccon
    srvPass = I.serverPassword $ I.server irccon
    setAppRootOrGuess =
      maybe fromRequest (hardcoded . T.encodeUtf8) (C.appRoot cfg)

wsApplication ::
     Pool SqlBackend -> I.Connection -> TChan W.Response -> Logger -> ServerApp
wsApplication pool irccon responseChan lgX pending =
  flip (Bunyan.withNamedLogger "thunderbuns.ws" id) lgX $ \lg -> do
    gc <- acceptRequest pending >>= W.guardedConnection
    runReaderT
      (logInfo "Connection accepted, starting ping/irc subscription threads")
      lg
    bracket
      (startThreads gc lg)
      (stopThreads lg)
      (const $ runReaderT (W.handleConn pool irccon gc) lg)
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
      Bunyan.withNamedLogger
        "thunderbuns.http"
        (requestLoggingContext req)
        (Bunyan.logDuration' $ \cb lg' ->
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
