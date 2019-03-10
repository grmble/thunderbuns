{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.WS.Handler where

import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as B
import Data.ByteString.D64.UUID (OrderedUUID, orderedUUID)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text.Encoding as T
import Data.UUID.V1 (nextUUID)
import Network.WebSockets (Connection, receiveData, sendPing, sendTextData)
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO (Bunyan, logDebug, logTrace)
import qualified Thunderbuns.Irc.Config as I
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Parser as I
import qualified Thunderbuns.Irc.Types as I
import Thunderbuns.Tlude
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
     forall r m. (Bunyan r m, MonadUnliftIO m)
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
    handleBytes :: L.ByteString -> EIO m W.Response
    handleBytes bs = do
      W.RequestWithID {rqid, rq} <- decodeData bs
      handleRequest irc rqid rq
    -- decode the json from the payload
    decodeData :: L.ByteString -> EIO m W.RequestWithID
    decodeData bs =
      stringError Nothing "Can not decode request: " `withExceptT`
      ExceptT (pure $ A.eitherDecode bs)
    -- helper for sending error responses
    errorResponse :: (Maybe W.RequestID, Text) -> W.Response
    errorResponse (rqid, msg) =
      case rqid of
        Nothing -> W.DecodeError msg
        Just rqid' -> W.GenericError rqid' msg

-- | Map a string error into the error type
stringError :: Maybe W.RequestID -> Text -> String -> (Maybe W.RequestID, Text)
stringError rqid msg err = (rqid, msg <> toText err)

-- | handle a valid request
handleRequest ::
     forall m. MonadUnliftIO m
  => I.Connection
  -> W.RequestID
  -> W.Request
  -> EIO m W.Response
handleRequest irc rqid = go
  where
    go :: W.Request -> EIO m W.Response
    go (W.GenericCommand cmd) = do
      cmd' <- parseCommand cmd
      sendCommand cmd'
      pure $ W.Done rqid
    go W.ChannelCommand {} = throwError (Just rqid, "IMPLEMENT ChannelCommand")
    parseCommand :: Text -> EIO m I.Command
    parseCommand cmd = do
      let bs = T.encodeUtf8 cmd
      stringError (Just rqid) "Error parsing IRC command: " `withExceptT`
        ExceptT (pure (parseOnly (I.parseCommand <* endOfInput) bs))
    --
    -- helper for sending commands
    sendCommand :: I.Command -> EIO m ()
    sendCommand cmd = do
      b <- liftIO (I.sendCommand irc cmd)
      unless b $
        throwError (Just rqid, "Can not send IRC command, server not connected")

-- | Send an IRC message over the websocket
sendResponse ::
     (Bunyan r m, MonadUnliftIO m)
  => I.ServerConfig
  -> GuardedConnection
  -> W.Response
  -> m ()
sendResponse _ gc response = do
  logTrace ("subscription message to websocket: " <> toText (A.encode response))
  sendGuardedTextData gc (A.encode response)

makeResponse :: MonadIO m => I.Message -> m W.Response
makeResponse msg = do
  uuid <- orderedUUID . fromJust <$> liftIO nextUUID
  pure $ responseFromMessage uuid msg

{--
   Because of IRC's Scandinavian origin, the characters {}|^ are
   considered to be the lower case equivalents of the characters []\~,
   respectively. This is a critical issue when determining the
   equivalence of two nicknames or channel names.

   XXX: normalize channel names by lowercasing like above
--}
responseFromMessage :: OrderedUUID -> I.Message -> W.Response
responseFromMessage uuid m@I.Message {msgPrefix, msgCmd, msgArgs} =
  fromMaybe (W.GenericMessage uuid (toText $ I.ircLine m)) $ do
    from <- parseFrom' (toText msgPrefix)
    cmd <-
      case msgCmd of
        I.Cmd x@"PRIVMSG" -> Just $ toText x
        I.Cmd x@"NOTICE" -> Just $ toText x
        _ -> Nothing
    channels <- validChannels
    let msg = toText $ B.intercalate " " $ tail msgArgs
    pure W.ChannelMessage {uuid, from, channels, cmd, msg}
  where
    parseFrom' :: Text -> Maybe W.From
    parseFrom' prefix =
      either (const Nothing) Just (Atto.parseOnly parseFrom prefix)
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
