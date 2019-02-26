{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Irc.Connection where

import Conduit (ConduitT, (.|), await, mapC, runConduit, yield)
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (Async, async, cancel, waitAnyCancel)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Conduit.Attoparsec as CA
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import System.Log.Bunyan
  ( Logger
  , Priority(..)
  , childLogger
  , logDebug
  , logRecord
  , modifyContext
  )
import Thunderbuns.Irc.Config
import Thunderbuns.Irc.Parser (ircCmdLine, ircLine, parseMessage, quoteLastArg)
import Thunderbuns.Irc.Types
import UnliftIO.Exception (bracket, throwString)
import UnliftIO.STM

-- | Create a new, disconnected connection
newConnection :: Server -> IO Connection
newConnection server = do
  status <- newTVarIO Disconnected
  fromServer <- newBroadcastTChanIO
  toServer <- newTBQueueIO 1
  handler <- newEmptyTMVarIO
  pure Connection {..}

runIrcConnection :: Connection -> Logger -> IO ()
runIrcConnection conn lgRoot = do
  lg <- childLogger "thunderbuns.irc" lgRoot
  bracket reserveConnection releaseConnection (handleConnection lg)
    --
    -- reserve the connection and start the handling threads
  where
    reserveConnection :: IO Bool
    reserveConnection = do
      tid <- myThreadId
      atomically $ do
        st <- readTVar (status conn)
        case st of
          Disconnected -> do
            writeTVar (status conn) Registration
            putTMVar (handler conn) tid
            pure True
          _ -> pure False
    --
    -- handle the connection
    handleConnection :: Logger -> Bool -> IO ()
    handleConnection lg isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      runTCPClient (uncurry clientSettings (connectionSettings $ server conn)) $ \tcpConn ->
        bracket
          (startBackgroundTasks lg tcpConn)
          (cancelBackgroundTasks lg)
          (waitForBackgroundTasks lg)
    --
    -- start the background tasks
    startBackgroundTasks lg tcpConn = do
      rgchan <- atomically $ dupTChan (fromServer conn)
      srcLg <- serverChildLogger lg "thunderbuns.irc.fromServer"
      a1 <- async $ runConduitFromServer srcLg (appSource tcpConn)
      sinkLg <- serverChildLogger lg "thunderbuns.irc.toServer"
      a2 <- async $ runConduitToServer sinkLg (appSink tcpConn)
      a3 <- async $ registerConnection rgchan
      pure [a1, a2, a3]
    --
    -- child loggers for various servers
    serverChildLogger :: Logger -> T.Text -> IO Logger
    serverChildLogger lg n =
      modifyContext (M.insert "server" (A.String $ host $ server conn)) <$> childLogger n lg
    -- wait for a shutdown because of socket closes
    -- first wait for end of registration task
    -- end of any ot the others means we go down
    waitForBackgroundTasks :: Logger -> [Async ()] -> IO ()
    waitForBackgroundTasks _ as = waitAnyCancel as $> ()
    cancelBackgroundTasks :: Logger -> [Async ()] -> IO ()
    cancelBackgroundTasks lg as = do
      logDebug "Canceling all background tasks" lg
      for_ as cancel
    -- register the connection
    registerConnection :: TChan Message -> IO ()
    registerConnection chan = do
      let to = toServer conn
      let n = T.encodeUtf8 $ nick $ server conn
      let fn = T.encodeUtf8 $ fullname $ server conn
      atomically $ writeTBQueue to (Command Nothing "NICK" [n])
      atomically $ writeTBQueue to (Command Nothing "USER" [n, "0", "*", fn])
      -- XXX: read result, 001 is good, 433 means nick has to be sent again
      atomically $ writeTVar (status conn) Connected
      pongPing n chan to
    pongPing :: ByteString -> TChan Message -> TBQueue Command-> IO ()
    pongPing nick chan to = do
      msg <- atomically $ readTChan chan
      case msgCmd msg of
        Cmd "PING" ->
          atomically $ writeTBQueue to (Command Nothing "PONG" [nick])
        _ -> pure ()
      pongPing nick chan to
    -- read from the server and broadcast the parsed messages
    runConduitFromServer :: Logger -> ConduitT () ByteString IO () -> IO ()
    runConduitFromServer lg src =
      runConduit $
      src .| CA.conduitParser parseMessage .| mapC snd .|
      sinkTChan (fromServer conn) lg
    -- from the command queue and send to server
    runConduitToServer :: Logger -> ConduitT ByteString Void IO () -> IO ()
    runConduitToServer lg sink =
      runConduit $ sourceTBQueue (toServer conn) lg .| mapC ircCmdLine .| sink
    --
    -- release the connection
    releaseConnection :: Bool -> IO ()
    releaseConnection _ =
      atomically $ do
        _ <- takeTMVar (handler conn)
        writeTVar (status conn) Disconnected

-- | Use a TBQueue as a conduit source.
sourceTBQueue ::
     MonadIO m => TBQueue Command -> Logger -> ConduitT () Command m ()
sourceTBQueue q lg = do
  a <- atomically $ readTBQueue q
  logRecord DEBUG id (ircCmdLine a) lg
  yield a
  sourceTBQueue q lg

-- | Use a TChan as a conduit sink.
sinkTChan :: MonadIO m => TChan Message -> Logger -> ConduitT Message Void m ()
sinkTChan chan lg =
  await >>= \case
    Nothing -> pure ()
    Just msg -> do
      logRecord DEBUG id (ircLine msg) lg
      atomically $ writeTChan chan msg
      sinkTChan chan lg

-- | Send a command to the irc server
--
-- Returns true if the message was queued, false if the server
-- is not currently connected (in which case the message was not queued)
sendCommand :: Connection -> Command -> IO Bool
sendCommand conn cmd =
  atomically (readTVar (status conn)) >>= \case
    Connected ->
      atomically (writeTBQueue (toServer conn) cmd) $> True
    _ -> pure False
