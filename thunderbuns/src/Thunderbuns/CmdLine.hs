{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.CmdLine where

import Control.Concurrent.Async (async, cancel)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall (auto, input)
import System.IO
import System.Log.Bunyan
import qualified Thunderbuns.Config as C
import Thunderbuns.Irc.Connection
import UnliftIO.Exception (bracket, catchIO)
import UnliftIO.MVar (MVar, newEmptyMVar, putMVar, takeMVar)

initialEnv :: IO C.Env
initialEnv = do
  envConfig <- input auto "./config.dhall"
  envLogQueue <- newEmptyMVar
  let pri = textToPriority (C.priority $ C.logging envConfig)
  envLogger <- rootLogger "thunderbuns.root" pri (putMVar envLogQueue)
  pure C.Env {..}

runMain :: IO ()
runMain = do
  C.Env {..} <- initialEnv
  withLogWriter envLogQueue (C.filename $ C.logging envConfig) $ do
    srv <- newConnection (C.server envConfig)
    runConnection srv envLogger

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
    logTo :: FilePath -> IO ()
    logTo "-" = go stderr
    logTo _ = do
      hPutStr stderr ("NOTE: log messages are being written to " <> fp <> "\n")
      withFile fp AppendMode $ \handle ->
        do hSetBuffering handle LineBuffering
           go handle
     `catchIO` const (logTo "=")
    go :: Handle -> IO ()
    go handle = do
      x <- takeMVar q
      LB.hPut handle (A.encode x <> "\n")
      go handle
