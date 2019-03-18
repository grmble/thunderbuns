{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.WS.Api
  ( GuardedConnection(..)
  , guardedConnection
  , sendGuardedTextData
  , sendGuardedPing
  , sendResponse
  , makeResponses
  , responseFromMessage
  ) where

import Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.ByteString.D64.UUID (OrderedUUID, orderedUUID)
import qualified Data.ByteString.Lazy as L
import Data.Foldable (fold)
import Data.Maybe (fromJust, fromMaybe)
import Data.Traversable (for)
import Data.UUID.V1 (nextUUID)
import Network.WebSockets (Connection, sendPing, sendTextData)
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO (Bunyan, logTrace)
import qualified Thunderbuns.Irc.Api as I (ircLowerCase, printMessage)
import qualified Thunderbuns.Irc.Parser as I (c2w)
import qualified Thunderbuns.Irc.Types as I (Cmd(..), Message(..))
import qualified Thunderbuns.WS.Types as W
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (bracket_)
import UnliftIO.MVar (MVar, newMVar, putMVar, takeMVar)

-- | A connection to a websocket client
--
-- It's guarded by an mvar to synchronize access
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

-- | Send an IRC message over the websocket
sendResponse ::
     (Bunyan r m, MonadUnliftIO m) => GuardedConnection -> W.Response -> m ()
sendResponse gc response = do
  logTrace ("subscription message to websocket: " <> toText (A.encode response))
  sendGuardedTextData gc (A.encode response)

-- | Make Websocket responses from a irc messages
makeResponses :: MonadIO m => [I.Message] -> m [W.Response]
makeResponses msgs =
  (compress . fold) <$>
                (for msgs $ \msg -> do
                    uuid <- orderedUUID . fromJust <$> liftIO nextUUID
                    pure $ responseFromMessage uuid msg)
  where
    compress (ms, []) = [W.GenericMessages ms]
    compress ([], ms) = [W.ChannelMessages ms]
    compress (gs, cs) = [W.GenericMessages gs, W.ChannelMessages cs]


responseFromMessage ::
     OrderedUUID -> I.Message -> ([W.GenericMessage], [W.ChannelMessage])
responseFromMessage uuid m@I.Message {msgPrefix, msgCmd, msgArgs} =
  fromMaybe ([W.GenericMessage uuid (toText $ I.printMessage m)], []) $ do
    from <- maybe Nothing W.runParseFrom (toText <$> msgPrefix)
    cmd <-
      case msgCmd of
        I.Cmd x@"PRIVMSG" -> Just $ toText x
        I.Cmd x@"NOTICE" -> Just $ toText x
        _ -> Nothing
    channels <- validChannels
    let msg = toText $ B.intercalate " " $ tail msgArgs
    pure $
      ( []
      , ((\x -> W.ChannelMessage {uuid, from, channel = x, cmd, msg}) <$>
         channels))
  where
    validChannelName :: B.ByteString -> Bool
    validChannelName bs = B.head bs `B.elem` "#&+!"
    validChannels :: Maybe [W.Channel]
    validChannels =
      case W.Channel . toText . I.ircLowerCase <$>
           filter validChannelName (B.split (I.c2w ',') (head msgArgs)) of
        [] -> Nothing
        x -> Just x
