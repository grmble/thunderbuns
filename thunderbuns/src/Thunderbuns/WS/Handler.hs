{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.WS.Handler where

import Control.Monad (forever)
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.WebSockets (Connection, receiveData, sendPing, sendTextData)
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO (MonadBunyan, logDebug, logTrace)
import qualified Thunderbuns.Irc.Config as I
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Parser as I
import qualified Thunderbuns.Irc.Types as I
import qualified Thunderbuns.WS.Types as W
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (bracket_)
import UnliftIO.MVar (MVar, newMVar, putMVar, takeMVar)

data GuardedConnection = GuardedConnection
  { conn :: Connection
  , mvar :: MVar ()
  }

guardedConnection :: MonadUnliftIO m => Connection -> m GuardedConnection
guardedConnection conn = do
  mvar <- newMVar ()
  pure GuardedConnection {conn, mvar}

sendGuardedTextData ::
     MonadUnliftIO m => GuardedConnection -> L.ByteString -> m ()
sendGuardedTextData GuardedConnection {conn, mvar} bs =
  bracket_ (takeMVar mvar) (putMVar mvar ()) (liftIO $ sendTextData conn bs)

sendGuardedPing :: MonadUnliftIO m => GuardedConnection -> L.ByteString -> m ()
sendGuardedPing GuardedConnection {conn, mvar} bs =
  bracket_ (takeMVar mvar) (putMVar mvar ()) (liftIO $ sendPing conn bs)

type EIO = ExceptT (Maybe W.RequestID, Text)

handleConn ::
     forall r m. (MonadBunyan r m, MonadUnliftIO m)
  => I.Connection
  -> GuardedConnection
  -> m ()
handleConn irc gc@GuardedConnection {conn} =
  forever $ do
    bs <- liftIO $ receiveData conn
    logDebug ("Received: " <> toText bs)
    resp <-
      either id id <$> runExceptT (errorResponse `withExceptT` handleBytes bs)
    logDebug ("Response: " <> toText (A.encode resp))
    sendGuardedTextData gc (A.encode resp)
  where
    handleBytes :: L.ByteString -> EIO m W.ResponseWithID
    handleBytes bs = do
      W.RequestWithID {rqid, rq} <- decodeData bs
      handleRequest irc (Just rqid) rq
    -- decode the json from the payload
    decodeData :: L.ByteString -> EIO m W.RequestWithID
    decodeData bs =
      stringError Nothing "Error decoding request" `withExceptT`
      ExceptT (pure $ A.eitherDecode bs)
    -- helper for sending error responses
    errorResponse :: (Maybe W.RequestID, Text) -> W.ResponseWithID
    errorResponse (rqid, msg) = W.ResponseWithID rqid (W.ErrorMessage msg)

-- | Map a string error into the error type
stringError :: Maybe W.RequestID -> Text -> String -> (Maybe W.RequestID, Text)
stringError rqid msg err = (rqid, msg <> toText err)

-- | handle a valid request
--
-- the request id is always present, but it's easier to pass around as a Maybe
handleRequest ::
     forall m. MonadUnliftIO m
  => I.Connection
  -> Maybe W.RequestID
  -> W.Request
  -> EIO m W.ResponseWithID
handleRequest irc rqid = go
  where
    go :: W.Request -> EIO m W.ResponseWithID
    go (W.GenericCommand cmd) = do
      cmd' <- parseCommand cmd
      sendCommand cmd'
      let own = fakeOwnFrom (I.server irc)
      pure $ W.ResponseWithID rqid (classifyIrcCommand own cmd')
    go W.ChannelCommand {} = throwError (rqid, "IMPLEMENT ChannelCommand")
    parseCommand :: Text -> EIO m I.Command
    parseCommand cmd = do
      let bs = T.encodeUtf8 cmd
      stringError rqid "Error parsing IRC command: " `withExceptT`
        ExceptT (pure (parseOnly (I.parseCommand <* endOfInput) bs))
    --
    -- helper for sending commands
    sendCommand :: I.Command -> EIO m ()
    sendCommand cmd = do
      b <- liftIO (I.sendCommand irc cmd)
      unless b $
        throwError (rqid, "Can not send IRC command, server not connected")

fakeOwnFrom :: I.ServerConfig -> W.From
fakeOwnFrom irc = W.From (W.Nick $ I.nick irc) (I.nick irc) "localhost"

-- | Send an IRC message over the websocket
sendResponse ::
     (MonadBunyan r m, MonadUnliftIO m)
  => I.ServerConfig
  -> GuardedConnection
  -> I.Message
  -> m ()
sendResponse irc gc msg = do
  let msg' = W.ResponseWithID Nothing (classifyIrcMessage (fakeOwnFrom irc) msg)
  logTrace ("subscription message to websocket: " <> toText (A.encode msg'))
  sendGuardedTextData gc (A.encode msg')


-- turns a command we sent to the server into a response message
classifyIrcCommand:: W.From -> I.Command -> W.Response
classifyIrcCommand myFrom I.Command {cmdPrefix, cmdCmd, cmdArgs} =
  classifyIrcMessage myFrom (I.Message cmdPrefix (I.Cmd cmdCmd) cmdArgs)

-- turns irc messages into GenericMessage or ChannelMessage / PrivateMessage
classifyIrcMessage :: W.From -> I.Message -> W.Response
classifyIrcMessage myFrom m@I.Message {msgPrefix, msgCmd, msgArgs} =
  fromMaybe (W.GenericMessage (toText $ I.ircLine m)) $ do
    from <- maybe (Just myFrom) parseFrom' (toText <$> msgPrefix)
    cmd <-
      case msgCmd of
        I.Cmd x@"PRIVMSG" -> Just $ toText x
        I.Cmd x@"NOTICE" -> Just $ toText x
        _ -> Nothing
    channels <- validChannels
    let msg = toText $ B.intercalate " " $ tail msgArgs
    pure W.ChannelMessage {from, channels, cmd, msg}
  where
    parseFrom' :: Text -> Maybe W.From
    parseFrom' prefix = either (const Nothing) Just (Atto.parseOnly parseFrom prefix)
    validChannelName :: B.ByteString -> Bool
    validChannelName bs = B.head bs `B.elem` "#&+!"
    validChannels :: Maybe [W.Channel]
    validChannels =
      case W.Channel . toText <$>
        filter validChannelName (B.split (I.c2w ',') (head msgArgs)) of
        [] -> Nothing
        x -> Just x

parseFrom :: Atto.Parser W.From
parseFrom =
  W.From <$> (W.Nick <$> Atto.takeWhile1 (/= '!') <* Atto.string "!") <*>
  (Atto.takeWhile1 (/= '@') <* Atto.string "@") <*>
  Atto.takeWhile1 (const True)
