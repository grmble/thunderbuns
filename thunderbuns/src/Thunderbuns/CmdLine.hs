{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.CmdLine where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (auto, input)
import Network.HTTP.Types (Status(..))
import Network.Wai (Application, Request(..), Response, responseStatus)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( Connection
  , ServerApp
  , acceptRequest
  , defaultConnectionOptions
  , receiveData
  , sendPing
  , sendTextData
  )
import System.IO
import System.Log.Bunyan
import System.Log.Bunyan.LogText
import qualified Thunderbuns.Config as C
import Thunderbuns.Irc.Connection
import UnliftIO.Exception (IOException, bracket, catchIO, finally)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

initialEnv :: IO C.Env
initialEnv = do
  envConfig <- input auto "./config.dhall"
  envLogQueue <- newEmptyMVar
  let pri = textToPriority (C.priority $ C.logging envConfig)
  envLogger <- rootLogger "thunderbuns.root" pri (putMVar envLogQueue)
  envIrcConnection <- newConnection (C.server envConfig)
  pure C.Env {..}

runMain :: IO ()
runMain = do
  C.Env {..} <- initialEnv
  withLogWriter envLogQueue (C.filename $ C.logging envConfig) $
    bracket
      (async $ runIrcConnection envIrcConnection envLogger)
      (logAndCancel "Canceling IRC Connection" envLogger)
      (const $ runWebServer (C.http envConfig) envLogger)

logAndCancel :: T.Text -> Logger -> Async a -> IO ()
logAndCancel msg lg a = do
  logInfo msg lg
  cancel a

runWebServer :: C.HttpConfig -> Logger -> IO ()
runWebServer C.HttpConfig {..} lg = do
  let p = fromIntegral port :: Int
  let dir = T.unpack staticDir :: FilePath
  lg' <- childLogger "thunderbuns.http" lg
  logInfo (T.pack $ "Serving HTTP on port " <> show p) lg'
  run p (mainApplication dir lg') `finally`
    logInfo (T.pack $ "Stopping HTTP on port " <> show p) lg

mainApplication :: FilePath -> Logger -> Application
mainApplication fp = logResponseTime serve
  where
    serve :: Logger -> Application
    serve lg =
      websocketsOr
        defaultConnectionOptions
        (wsApplication lg)
        (staticApp $ defaultWebAppSettings fp)

wsApplication :: Logger -> ServerApp
wsApplication lgRoot pending = do
  lg <- childLogger "thunderbuns.ws" lgRoot
  conn <- acceptRequest pending
  logInfo "Connection accepted, starting ping thread" lg
  bracket (async $ pingThread conn 0)
    (logAndCancel "Stopping ping thread on ws connection" lg)
    (\_ -> handleConn lg conn)
  where
    handleConn :: Logger -> Connection -> IO ()
    handleConn lg conn = do
      x <- receiveData conn :: IO Text
      logDebug ("received: " <> x) lg
      sendTextData conn x
      handleConn lg conn
    pingThread :: Connection -> Int -> IO ()
    pingThread conn !n = do
      threadDelay (10 * 1000 * 1000)
      sendPing conn ("ping " <> toText (show n))
      pingThread conn (n + 1)

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
