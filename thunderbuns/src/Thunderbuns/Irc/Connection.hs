{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Irc.Connection where

import qualified Data.Text.Encoding as T
import System.Log.Bunyan.RIO (Bunyan, logDebug)
import Thunderbuns.Irc.Config
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
import UnliftIO.Exception (finally)
import UnliftIO.STM

-- | Create a new, disconnected connection
newConnection ::
     (HasServerConfig r, MonadReader r m, MonadIO m) => m IrcConnection
newConnection = do
  server <- view serverConfig
  status <- newTVarIO Disconnected
  fromServer <- newBroadcastTChanIO
  toServer <- newTBQueueIO 1
  handler <- newEmptyTMVarIO
  pure IrcConnection {..}

-- | register the connection
registerConnection ::
     (HasIrcConnection r, HasServerConfig r, Bunyan r m, MonadUnliftIO m)
  => TChan Message
  -> TBQueue Command
  -> m ()
registerConnection msgChan cmdQueue = do
  conn <- view ircConnection
  ServerConfig {nick, fullname, serverPassword} <- view serverConfig
  let n = T.encodeUtf8 nick
  let fn = T.encodeUtf8 fullname
  for_ (T.encodeUtf8 <$> serverPassword) $ \p ->
    atomically $ writeTBQueue cmdQueue (Command "PASS" [p])
  atomically $ writeTBQueue cmdQueue (Command "NICK" [n])
  atomically $ writeTBQueue cmdQueue (Command "USER" [n, "0", "*", fn])
  -- XXX
  -- how we should do this:
  -- drain queue with 10-15 seconds timeout, see if it has 001 message (welcome)
  -- if good, continue, otherwise terminate
  --
  -- anyway, posting 2 messages in 1 transaction to a bounded queue seems to hang ...
  -- who woulda thunk
  {--
  msg <- atomically $ readTChan msgChan
  for_ (nicksrvPassword $ server conn) $ \p ->
    atomically
      (writeTBQueue
          cmdQueue
          (Command "PRIVMSG" ["NickServ", "IDENTIFY " <> T.encodeUtf8 p]))
  --}
  for_ (channels $ server conn) $ \cn ->
    atomically (writeTBQueue cmdQueue (Command "JOIN" [T.encodeUtf8 cn]))
  atomically $ writeTVar (status conn) Connected
  pongPing msgChan cmdQueue

pongPing ::
     (Bunyan r m, MonadUnliftIO m)
  => TChan Message
  -> TBQueue Command
  -> m ()
pongPing msgChan cmdQueue =
  finally -- infix syntax looks horrible after hindent
    (forever $ do
       msg <- atomically $ readTChan msgChan
       case msgCmd msg of
         Cmd "PING" ->
           atomically $ writeTBQueue cmdQueue (Command "PONG" (msgArgs msg))
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
sendCommand ::
     (HasIrcConnection r, MonadReader r m, MonadUnliftIO m) => Command -> m Bool
sendCommand cmd = do
  conn <- view ircConnection
  atomically $
    readTVar (status conn) >>= \case
      Connected -> do
        writeTBQueue (toServer conn) cmd
        writeTChan (fromServer conn) (fakeMessage conn)
        pure True
      _ -> pure False
  where
    fakeMessage conn =
      Message
        { msgPrefix = Just (fakePrefix conn)
        , msgCmd = Cmd (cmdCmd cmd)
        , msgArgs = cmdArgs cmd
        }
    -- XXX we should track the servers prefix for us
    fakePrefix :: IrcConnection -> ByteString
    fakePrefix conn =
      let n = T.encodeUtf8 $ nick (server conn)
       in n <> "!" <> n <> "@localhost"
