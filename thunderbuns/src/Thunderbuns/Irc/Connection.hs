{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Irc.Connection where

import qualified Data.Text.Encoding as T
import System.Log.Bunyan.RIO (Bunyan, logDebug)
import Thunderbuns.Irc.Config
import Thunderbuns.Irc.Parser (ircLine)
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
import UnliftIO (MonadIO(..), MonadUnliftIO(..))
import UnliftIO.Exception (finally, throwString)
import UnliftIO.STM

-- | Create a new, disconnected connection
newConnection :: MonadIO m => ServerConfig -> m Connection
newConnection server = do
  status <- newTVarIO Disconnected
  fromServer <- newBroadcastTChanIO
  toServer <- newTBQueueIO 1
  handler <- newEmptyTMVarIO
  pure Connection {..}
    -- | register the connection

registerConnection ::
     (Bunyan r m, MonadUnliftIO m)
  => Connection
  -> TChan Message
  -> TBQueue Command
  -> m ()
registerConnection conn msgChan cmdQueue = do
  let ServerConfig {nick, fullname, serverPassword} = server conn
  let n = T.encodeUtf8 nick
  let fn = T.encodeUtf8 fullname
  for_ (T.encodeUtf8 <$> serverPassword) $ \p ->
    atomically $ writeTBQueue cmdQueue (Command "PASS" [p])
  atomically $ writeTBQueue cmdQueue (Command "NICK" [n])
  atomically $ writeTBQueue cmdQueue (Command "USER" [n, "0", "*", fn])
  msg <- atomically $ readTChan msgChan
  case msgCmd msg of
    Response RplWelcome ->
      atomically $ do
      for_ (nicksrvPassword $ server conn) $ \p ->
        writeTBQueue cmdQueue (Command "PRIVMSG" ["NickServ", "IDENTIFY", T.encodeUtf8 p])
      for_ (channels $ server conn) $ \cn ->
        writeTBQueue cmdQueue (Command "JOIN" [T.encodeUtf8 cn])
    _ ->
      throwString
        ("Unexpected message, waiting for 001, got: " <> show (ircLine msg))
  atomically $ writeTVar (status conn) Connected
  pongPing n msgChan cmdQueue

pongPing ::
     (Bunyan r m, MonadUnliftIO m)
  => ByteString
  -> TChan Message
  -> TBQueue Command
  -> m ()
pongPing nick msgChan cmdQueue =
  finally -- infix syntax looks horrible after hindent
    (forever $ do
       msg <- atomically $ readTChan msgChan
       case msgCmd msg of
         Cmd "PING" ->
           atomically $ writeTBQueue cmdQueue (Command "PONG" [nick])
         _ -> pure ())
    (logDebug "IRC Pongping thread terminated.")

-- | Send a command to the irc server
--
-- Returns true if the message was queued, false if the server
-- is not currently connected (in which case the message was not queued)
--
-- This also fake a server reply for the command (because the
-- server does not send us our own messages back).
-- This way the same path is taken for our own commands back into
-- the client.
--
-- Exception:  the code handling registration does NOT use this
-- function, it just sends to the server.  This is good because
-- this way the passwords involved are less exposed.
sendCommand :: MonadUnliftIO m => Connection -> Command -> m Bool
sendCommand conn cmd =
  atomically $
  readTVar (status conn) >>= \case
    Connected -> do
      writeTBQueue (toServer conn) cmd
      writeTChan (fromServer conn) (fakeMessage conn cmd)
      pure True
    _ -> pure False

-- | Fake a message from a command
fakeMessage :: Connection -> Command -> Message
fakeMessage conn cmd =
  Message
    { msgPrefix = Just (fakePrefix conn)
    , msgCmd = Cmd (cmdCmd cmd)
    , msgArgs = cmdArgs cmd
    }

-- XXX we should track the servers prefix for us
fakePrefix :: Connection -> ByteString
fakePrefix conn =
  let n = T.encodeUtf8 $ nick (server conn)
    in n <> "!" <> n <> "@localhost"
