module Thunderbuns.Irc.Connection where

import Conduit
  ( ConduitM(..)
  , ConduitT(..)
  , (.|)
  , await
  , mapC
  , runConduit
  , yield
  )
import Control.Concurrent.Async (Async, async, waitAnyCancel, waitBoth, wait)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import System.Log.Bunyan
import Thunderbuns.Irc.Config (Server(..), connectionSettings)
import Thunderbuns.Irc.Parser (Message(..), ircLine, parseMessage, quoteLastArg)
import UnliftIO.Exception (bracket, throwString)
import UnliftIO.STM
  ( TBQueue
  , TChan
  , TVar
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , newTBQueueIO
  , newTVarIO
  , readTBQueue
  , readTChan
  , readTVar
  , writeTBQueue
  , writeTChan
  , writeTVar
  )

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
newConnection srv = do
  status <- newTVarIO Disconnected
  fromServer <- newBroadcastTChanIO
  toServer <- newTBQueueIO 1
  pure Connection {server = srv, fromServer, toServer, status}

runConnection :: Connection -> Logger -> IO ()
runConnection conn lg =
  bracket reserveConnection handleConnection releaseConnection
    --
    -- reserve the connection and start the handling threads
  where
    reserveConnection :: IO Bool
    reserveConnection =
      atomically $ do
        st <- readTVar (status conn)
        case st of
          Disconnected -> do
            writeTVar (status conn) Registration
            pure True
          _ -> pure False
    --
    -- handle the connection
    handleConnection :: Bool -> IO ()
    handleConnection isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      runTCPClient (uncurry clientSettings (connectionSettings $ server conn)) $ \tcpConn -> do
        logDebug "connection established" lg
        bracket
          (startBackgroundTasks tcpConn)
          (\(_,rg) -> wait rg)
          (\(xs,_) -> waitAnyCancel xs $> ())
    startBackgroundTasks tcpConn = do
      rgchan <- atomically $ dupTChan (fromServer conn)
      a4 <- async $ registerConnection rgchan
      a1 <- async $ runConduitFromServer $ appSource tcpConn
      a2 <- async $ runConduitToServer $ appSink tcpConn
      -- a3 <- async $ logFromServer rdchan
      pure ([a1, a2], a4)
    -- register the connection
    registerConnection :: TChan Message -> IO ()
    registerConnection chan = do
      let from = fromServer conn
      let to = toServer conn
      let n = T.encodeUtf8 $ nick $ server conn
      let fn = T.encodeUtf8 $ fullname $ server conn
      logDebug "registerConnection ... writing to tbqueue" lg
      atomically $ writeTBQueue to (Command Nothing "NICK" [n])
      atomically $ writeTBQueue to (Command Nothing "USER" [n, "0", "*", fn])
      -- XXX: read result, 001 is good, 433 means nick has to be sent again
      pure ()
    -- read from the server and broadcast the parsed messages
    runConduitFromServer :: ConduitT () ByteString IO () -> IO ()
    runConduitFromServer src = do
      logDebug "runConduitFromServer" lg
      runConduit $
        src .| CA.conduitParser parseMessage .| mapC snd .|
        sinkTChan (fromServer conn) lg
      logDebug "DONE runConduitFromServer" lg
    -- from the command queue and send to server
    runConduitToServer :: ConduitT ByteString Void IO () -> IO ()
    runConduitToServer sink = do
      logDebug "runConduitToServer" lg
      runConduit $ sourceTBQueue (toServer conn) lg .| mapC ircCmdLine .| sink
      logDebug "DONE runConduitToServer" lg
    --
    -- log from server
    logFromServer :: TChan Message -> IO ()
    logFromServer chan = do
      msg <- atomically $ readTChan chan
      logRecord DEBUG M.empty ("LOG: " <> ircLine msg) lg
      logFromServer chan
    --
    -- release the connection
    releaseConnection :: Bool -> IO ()
    releaseConnection _ = atomically $ writeTVar (status conn) Disconnected

-- | Use a TBQueue as a conduit source.
sourceTBQueue ::
     MonadIO m => TBQueue Command -> Logger -> ConduitT () Command m ()
sourceTBQueue q lg = do
  logDebug "sourceTBQueue... reading from tbqueue" lg
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
      logRecord DEBUG M.empty ("sinkTChan: " <> show msg) lg
      atomically $ writeTChan chan msg
      sinkTChan chan lg
