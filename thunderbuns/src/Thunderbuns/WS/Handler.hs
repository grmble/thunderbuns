{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.WS.Handler where

import Control.Monad (forever)
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import qualified Data.ByteString.Lazy as L
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.WebSockets (Connection, receiveData, sendPing, sendTextData)
import System.Log.Bunyan (Logger, logDebug)
import System.Log.Bunyan.LogText (toText)
import qualified Thunderbuns.Irc.Connection as I
import qualified Thunderbuns.Irc.Parser as I
import qualified Thunderbuns.Irc.Types as I
import qualified Thunderbuns.WS.Types as W
import UnliftIO.Exception (bracket_)
import UnliftIO.MVar (MVar, newMVar, putMVar, takeMVar)

data GuardedConnection = GuardedConnection
  { conn :: Connection
  , mvar :: MVar ()
  }

guardedConnection :: Connection -> IO GuardedConnection
guardedConnection conn = do
  mvar <- newMVar ()
  pure GuardedConnection {..}

sendGuardedTextData :: GuardedConnection -> L.ByteString -> IO ()
sendGuardedTextData GuardedConnection {..} bs =
  bracket_ (takeMVar mvar) (putMVar mvar ()) (sendTextData conn bs)

sendGuardedPing :: GuardedConnection -> L.ByteString -> IO ()
sendGuardedPing GuardedConnection {..} bs =
  bracket_ (takeMVar mvar) (putMVar mvar ()) (sendPing conn bs)

type EIO = ExceptT (Maybe W.RequestID, Text) IO

handleConn :: I.Connection -> Logger -> GuardedConnection -> IO ()
handleConn irc lg gc@GuardedConnection{conn} =
  forever $ do
    bs <- receiveData conn
    logDebug ("Received: " <> toText bs) lg
    resp <-
      either id id <$> runExceptT (errorResponse `withExceptT` handleBytes bs)
    logDebug ("Response: " <> toText (A.encode resp)) lg
    sendGuardedTextData gc (A.encode resp)
  where
    handleBytes :: L.ByteString -> EIO W.ResponseWithID
    handleBytes bs = do
      W.RequestWithID {..} <- decodeData bs
      handleRequest irc (Just rqid) rq
    -- decode the json from the payload
    decodeData :: L.ByteString -> EIO W.RequestWithID
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
     I.Connection -> Maybe W.RequestID -> W.Request -> EIO W.ResponseWithID
handleRequest irc rqid = go
  where
    go :: W.Request -> EIO W.ResponseWithID
    go (W.GenericCommand cmd) = do
      cmd' <- parseCommand cmd
      sendCommand cmd'
      pure $ W.ResponseWithID rqid $ W.GenericMessage cmd
    go W.ChannelCommand {} = throwError (rqid, "IMPLEMENT ChannelCommand")
    parseCommand :: Text -> EIO I.Command
    parseCommand cmd = do
      let bs = T.encodeUtf8 cmd
      stringError rqid "Error parsing IRC command: " `withExceptT`
        ExceptT (pure (parseOnly (I.parseCommand <* endOfInput) bs))
    --
    -- helper for sending commands
    sendCommand :: I.Command -> EIO ()
    sendCommand cmd = do
      b <- liftIO (I.sendCommand irc cmd)
      unless b $
        throwError (rqid, "Can not send IRC command, server not connected")

-- | Send an IRC message over the websocket
sendResponse :: Logger -> GuardedConnection -> I.Message -> IO ()
sendResponse lg gc msg = do
  let msg' = W.ResponseWithID Nothing $ W.GenericMessage (toText $ I.ircLine msg)
  logDebug ("A message from our server: " <> toText (A.encode msg')) lg
  sendGuardedTextData gc (A.encode msg')
