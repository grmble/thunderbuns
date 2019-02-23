{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Irc.Connection where

import Conduit (ConduitT, (.|), await, mapC, runConduit, yield)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, async, cancel, wait, waitAnyCancel)
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
  )
import Thunderbuns.Irc.Config (Server(..), connectionSettings)
import Thunderbuns.Irc.Parser (Message(..), ircLine, parseMessage, quoteLastArg)
import UnliftIO.Exception (bracket, throwString)
import UnliftIO.STM

-- | IrcServer Connection status
data Status
  = Disconnected
  | Registration
  | Connected

-- | IrcServer connection
data Connection = Connection
  { server :: !Server
  , fromServer :: !(TChan Message)
  , toServer :: !(TBQueue Command)
  , status :: !(TVar Status)
  , handler :: !(TMVar ThreadId)
  }

-- | A command to the irc server
data Command = Command
  { cmdPrefix :: !(Maybe ByteString)
  , cmdCmd :: !ByteString
  , cmdArgs :: ![ByteString]
  } deriving (Eq, Show)

ircCmdLine :: Command -> ByteString
ircCmdLine (Command pre cmd args) =
  let lst = maybe [] (pure . (<>) ":") pre ++ cmd : args
   in B.intercalate " " (quoteLastArg lst) <> "\r\n"

-- | Create a new, disconnected connection
newConnection :: Server -> IO Connection
newConnection server = do
  status <- newTVarIO Disconnected
  fromServer <- newBroadcastTChanIO
  toServer <- newTBQueueIO 1
  handler <- newEmptyTMVarIO
  pure Connection {..}

runConnection :: Connection -> Logger -> IO ()
runConnection conn lg =
  bracket reserveConnection releaseConnection handleConnection
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
    handleConnection :: Bool -> IO ()
    handleConnection isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      runTCPClient (uncurry clientSettings (connectionSettings $ server conn)) $ \tcpConn ->
        bracket
          (startBackgroundTasks tcpConn)
          cancelBackgroundTasks
          waitForBackgroundTasks
    --
    -- start the background tasks
    startBackgroundTasks tcpConn = do
      rgchan <- atomically $ dupTChan (fromServer conn)
      srcLg <- serverChildLogger "thunderbuns.fromServer"
      a1 <- async $ runConduitFromServer srcLg (appSource tcpConn)
      sinkLg <- serverChildLogger "thunderbuns.toServer"
      a2 <- async $ runConduitToServer sinkLg (appSink tcpConn)
      -- a3 <- async $ logFromServer rdchan
      a4 <- async $ registerConnection rgchan
      pure ([a1, a2], a4)
    --
    -- child loggers for various servers
    serverChildLogger :: T.Text -> IO Logger
    serverChildLogger n = childLogger n (M.singleton "server" (A.String (host $ server conn))) lg
    -- wait for a shutdown because of socket closes
    -- first wait for end of registration task
    -- end of any ot the others means we go down
    waitForBackgroundTasks :: ([Async ()], Async ()) -> IO ()
    waitForBackgroundTasks (as, rg) = do
      wait rg
      waitAnyCancel as $> ()
    cancelBackgroundTasks :: ([Async ()], Async ()) -> IO ()
    cancelBackgroundTasks (as, rg) = do
      logDebug "Canceling all background tasks" lg
      cancel rg
      for_ as cancel
    -- register the connection
    registerConnection :: TChan Message -> IO ()
    registerConnection chan = do
      let from = fromServer conn
      let to = toServer conn
      let n = T.encodeUtf8 $ nick $ server conn
      let fn = T.encodeUtf8 $ fullname $ server conn
      atomically $ writeTBQueue to (Command Nothing "NICK" [n])
      atomically $ writeTBQueue to (Command Nothing "USER" [n, "0", "*", fn])
      -- XXX: read result, 001 is good, 433 means nick has to be sent again
      pure ()
    -- read from the server and broadcast the parsed messages
    runConduitFromServer :: Logger -> ConduitT () ByteString IO () -> IO ()
    runConduitFromServer lg src = do
      runConduit $
        src .| CA.conduitParser parseMessage .| mapC snd .|
        sinkTChan (fromServer conn) lg
    -- from the command queue and send to server
    runConduitToServer :: Logger -> ConduitT ByteString Void IO () -> IO ()
    runConduitToServer lg sink = do
      runConduit $ sourceTBQueue (toServer conn) lg .| mapC ircCmdLine .| sink
    --
    -- release the connection
    releaseConnection :: Bool -> IO ()
    releaseConnection _ = atomically $ do
      _ <- takeTMVar (handler conn)
      writeTVar (status conn) Disconnected

-- | Use a TBQueue as a conduit source.
sourceTBQueue ::
     MonadIO m => TBQueue Command -> Logger -> ConduitT () Command m ()
sourceTBQueue q lg = do
  a <- atomically $ readTBQueue q
  logRecord DEBUG M.empty (ircCmdLine a) lg
  yield a
  sourceTBQueue q lg

-- | Use a TChan as a conduit sink.
sinkTChan :: MonadIO m => TChan Message -> Logger -> ConduitT Message Void m ()
sinkTChan chan lg =
  await >>= \case
    Nothing -> pure ()
    Just msg -> do
      logRecord DEBUG M.empty (ircLine msg) lg
      atomically $ writeTChan chan msg
      sinkTChan chan lg
