-- | Call in from the command line
--
-- Here all the parts are wired together:
-- WAI, WebSockets, connection to the IRC network.
--
-- No MonadBunyan here - it's easiest to stay in IO
-- and call into the various other stacks.
module Thunderbuns.CmdLine where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (auto, input)
import Network.HTTP.Types (Status(..))
import Network.Wai (Application, Request(..), Response, responseStatus)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, defaultConnectionOptions)
import System.IO
import System.Log.Bunyan
import System.Log.Bunyan.LogText
import qualified Thunderbuns.Config as C
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Types as I
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
  localLogger "thunderbuns.http" id $ \lg -> do
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
        (staticApp $ defaultWebAppSettings fp)

wsApplication :: I.Connection -> Logger -> ServerApp
wsApplication irccon lgX pending =
  flip (localLogger "thunderbuns.ws" id) lgX $ \lg -> do
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
        runReaderT (sendResponse gc msg) lg

--
-- XXX consider promoting these to bunyan
--
textToPriority :: Text -> Priority
textToPriority "FATAL" = FATAL
textToPriority "ERROR" = ERROR
textToPriority "WARN" = WARN
textToPriority "INFO" = INFO
textToPriority "DEBUG" = DEBUG
textToPriority "TRACE" = TRACE
textToPriority _ = INFO

-- | Run the action while running a log writing thread in the background
--
-- The log writer will write the entries from the mvar to the file.
-- A filepath of "-" is used for stderr.
--
-- If an error occurs writing to the file, it will try to use stderr
withLogWriter :: MVar A.Object -> FilePath -> IO a -> IO a
withLogWriter q fp action = bracket (async $ logTo fp) cancel (const action)
  where
    handleError :: IOException -> IO ()
    handleError e = do
      hPutStrLn stderr ("Exception: " <> show e)
      go stderr
    logTo :: FilePath -> IO ()
    logTo "-" = go stderr
    logTo _ =
      do hPutStr
           stderr
           ("NOTE: log messages are being written to " <> fp <> "\n")
         withFile fp AppendMode $ \handle -> do
           hSetBuffering handle LineBuffering
           go handle
     `catchIO` handleError
    go :: Handle -> IO ()
    go handle = do
      x <- takeMVar q
      LB.hPut handle (A.encode x <> "\n")
      go handle

localLogger ::
     MonadUnliftIO m
  => Text
  -> (A.Object -> A.Object)
  -> (Logger -> m a)
  -> Logger
  -> m a
localLogger n ctxfn action lg = do
  lg' <- childLogger' n ctxfn lg
  action lg'

-- XXX fix in bunyan (improve logDuration with a final modification of the context)
logResponseTime :: (Logger -> Application) -> Logger -> Application
logResponseTime app lg req respond = do
  start <- getLoggingTime
  lg' <- childLogger' "thunderbuns.http" (requestLoggingContext req) lg
  app lg' req $ \res -> do
    responded <- respond res
    end <- getLoggingTime
    let (fobj, msg) = duration start end
    let fobj' = responseLoggingContext res
    logRecord INFO (fobj' . fobj) msg lg'
    pure responded

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
