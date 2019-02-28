{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Irc.Connection where

import Conduit
  ( ConduitT
  , MonadThrow
  , (.|)
  , await
  , lift
  , mapC
  , runConduit
  , yield
  )
import Control.Concurrent (myThreadId)
import Control.Exception (SomeException)
import Control.Monad (forever, unless)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Network.TLS as CT
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO
  ( MonadBunyan
  , Priority(..)
  , localLogger
  , logDebug
  , logRecord
  )
import Thunderbuns.Irc.Config
import Thunderbuns.Irc.Parser (ircCmdLine, ircLine, parseMessageOrLine)
import Thunderbuns.Irc.Types
import UnliftIO (MonadIO(..), MonadUnliftIO(..))
import UnliftIO.Async (Async, async, cancel, waitAnyCatchCancel)
import UnliftIO.Exception (bracket, throwString)
import UnliftIO.STM

-- | Create a new, disconnected connection
newConnection :: MonadIO m => ServerConfig -> m Connection
newConnection server = do
  status <- newTVarIO Disconnected
  fromServer <- newBroadcastTChanIO
  toServer <- newTBQueueIO 1
  handler <- newEmptyTMVarIO
  pure Connection {..}

runIrcConnection ::
     forall r m. (MonadThrow m, MonadUnliftIO m, MonadBunyan r m)
  => Connection
  -> m ()
runIrcConnection conn =
  localLogger "thunderbuns.irc" id $
  bracket reserveConnection releaseConnection handleConnection
    --
    -- reserve the connection and start the handling threads
  where
    reserveConnection :: m Bool
    reserveConnection = do
      tid <- liftIO myThreadId
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
    handleConnection :: Bool -> m ()
    handleConnection isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      logDebug "establishing connection to irc server"
      let cfg = uncurry CT.tlsClientConfig (connectionSettings $ server conn)
      CT.runTLSClient (cfg {CT.tlsClientUseTLS = tls $ server conn}) $ \tcpConn ->
        bracket
          (startBackgroundTasks tcpConn)
          cancelBackgroundTasks
          waitForBackgroundTasks
    --
    -- start the background tasks
    startBackgroundTasks :: CN.AppData -> m [Async ()]
    startBackgroundTasks tcpConn = do
      rgchan <- atomically $ dupTChan (fromServer conn)
      a1 <-
        async $
        serverLogger "thunderbuns.irc.fromServer" $
        runConduitFromServer (CN.appSource tcpConn)
      a2 <-
        async $
        serverLogger "thunderbuns.irc.toServer" $
        runConduitToServer (CN.appSink tcpConn)
      a3 <- async $ registerConnection rgchan
      pure [a1, a2, a3]
    --
    -- child loggers for various servers
    serverLogger :: T.Text -> m a -> m a
    serverLogger n =
      localLogger n (M.insert "server" (A.String $ host $ server conn))
    -- wait for a shutdown because of socket closes
    -- first wait for end of registration task
    -- end of any ot the others means we go down
    waitForBackgroundTasks :: [Async ()] -> m ()
    waitForBackgroundTasks as =
      waitAnyCatchCancel as >>= \case
        (_, Right ()) -> pure ()
        (_, Left ex) ->
          logRecord
            ERROR
            (exceptionContext ex)
            ("Exception caught from background task: " <> show ex)
    cancelBackgroundTasks :: [Async ()] -> m ()
    cancelBackgroundTasks as = do
      logDebug "Canceling all background tasks"
      for_ as cancel
    -- register the connection
    registerConnection :: TChan Message -> m ()
    registerConnection chan = do
      let to = toServer conn
      let n = T.encodeUtf8 $ nick $ server conn
      let fn = T.encodeUtf8 $ fullname $ server conn
      atomically $ writeTBQueue to (Command Nothing "NICK" [n])
      atomically $ writeTBQueue to (Command Nothing "USER" [n, "0", "*", fn])
      -- XXX: read result, 001 is good, 433 means nick has to be sent again
      atomically $ writeTVar (status conn) Connected
      pongPing n chan to
    pongPing :: ByteString -> TChan Message -> TBQueue Command -> m ()
    pongPing nick chan to = do
      msg <- atomically $ readTChan chan
      case msgCmd msg of
        Cmd "PING" ->
          atomically $ writeTBQueue to (Command Nothing "PONG" [nick])
        _ -> pure ()
      pongPing nick chan to
    -- read from the server and broadcast the parsed messages
    runConduitFromServer :: ConduitT () ByteString m () -> m ()
    runConduitFromServer src =
      runConduit $
      src .| CA.conduitParser parseMessageOrLine .| mapC snd .| filterErrorLines .|
      sinkTChan (fromServer conn)
    -- filter out and log error lines
    filterErrorLines =
      forever $
      await >>= \case
        Nothing -> pure ()
        Just (Left bs) ->
          lift $
          logRecord
            WARN
            (M.insert "line" (A.String $ toText bs))
            ("Can not parse line: " <> toText (show bs))
        Just (Right msg) -> yield msg
    -- from the command queue and send to server
    runConduitToServer :: ConduitT ByteString Void m () -> m ()
    runConduitToServer sink =
      runConduit $ sourceTBQueue (toServer conn) .| mapC ircCmdLine .| sink
    --
    -- release the connection
    releaseConnection :: Bool -> m ()
    releaseConnection _ =
      atomically $ do
        _ <- takeTMVar (handler conn)
        writeTVar (status conn) Disconnected

-- | Use a TBQueue as a conduit source.
sourceTBQueue ::
     (MonadUnliftIO m, MonadBunyan r m)
  => TBQueue Command
  -> ConduitT () Command m ()
sourceTBQueue q = do
  a <- atomically $ readTBQueue q
  lift $ logRecord DEBUG id (ircCmdLine a)
  yield a
  sourceTBQueue q

-- | Use a TChan as a conduit sink.
sinkTChan ::
     (MonadUnliftIO m, MonadBunyan r m)
  => TChan Message
  -> ConduitT Message Void m ()
sinkTChan chan =
  await >>= \case
    Nothing -> pure ()
    Just msg -> do
      lift $ logRecord DEBUG id (ircLine msg)
      atomically $ writeTChan chan msg
      sinkTChan chan

-- | Send a command to the irc server
--
-- Returns true if the message was queued, false if the server
-- is not currently connected (in which case the message was not queued)
sendCommand :: MonadUnliftIO m => Connection -> Command -> m Bool
sendCommand conn cmd =
  atomically (readTVar (status conn)) >>= \case
    Connected -> atomically (writeTBQueue (toServer conn) cmd) $> True
    _ -> pure False

--- XXX promote
exceptionContext :: SomeException -> A.Object -> A.Object
exceptionContext ex =
  M.insert "err" (A.Object $ M.singleton "msg" (A.String (T.pack $ show ex)))
