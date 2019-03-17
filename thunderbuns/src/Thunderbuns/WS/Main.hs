{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.WS.Main where

import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import Network.WebSockets (receiveData)
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO (Bunyan, logDebug)
import Thunderbuns.Config (HasDatabasePool)
import Thunderbuns.Irc.Api (HasIrcConnection, sendCommand)
import qualified Thunderbuns.Irc.Parser as I
import qualified Thunderbuns.Irc.Types as I
import Thunderbuns.WS.Api
import Thunderbuns.Persist.Api (selectChannelBefore, withSqlBackend)
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Types as W
import UnliftIO (MonadUnliftIO(..))

-- | Internal error handling type
type EIO = ExceptT (Maybe W.RequestID, Text)

-- | Handle the websocket connection
handleConn ::
     forall r m. (HasDatabasePool r, HasIrcConnection r, Bunyan r m, MonadUnliftIO m)
  => GuardedConnection
  -> m ()
handleConn gc@GuardedConnection {conn} =
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
      handleRequest gc rqid rq
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
     forall r m. (HasIrcConnection r, HasDatabasePool r, Bunyan r m, MonadUnliftIO m)
  => GuardedConnection
  -> W.RequestID
  -> W.Request
  -> EIO m W.Response
handleRequest gc rqid = go
  where
    go :: W.Request -> EIO m W.Response
    go (W.GenericCommand cmd) = do
      parseCommand cmd >>= sendCommand'
      pure $ W.Done rqid
    go W.GetChannelMessages {channel, before} = do
      msgs <- withSqlBackend (selectChannelBefore channel before)
      for_ msgs (liftIO . sendGuardedTextData gc . A.encode)
      pure $ W.Done rqid
    parseCommand :: Text -> EIO m I.Command
    parseCommand cmd = do
      let bs = T.encodeUtf8 cmd
      stringError (Just rqid) "Error parsing IRC command: " `withExceptT`
        ExceptT (pure (parseOnly (I.parseCommand <* endOfInput) bs))
    --
    -- helper for sending commands
    sendCommand' :: I.Command -> EIO m ()
    sendCommand' cmd = do
      b <- lift $ sendCommand cmd
      unless b $
        throwError (Just rqid, "Can not send IRC command, server not connected")
