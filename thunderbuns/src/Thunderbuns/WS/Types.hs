{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | WS Types
--
-- These are the types that go over the (websocket) wire.
--
module Thunderbuns.WS.Types where

import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as Atto
import Data.ByteString.D64.UUID (OrderedUUID)
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import qualified Data.Scientific as SC
import qualified Data.Text as T
import Thunderbuns.Tlude

-- | Request defines anything that can be received from the frontend
--
-- Note that the backend will never see a request without id
data RequestWithID = RequestWithID
  { rqid :: RequestID
  , rq :: Request
  } deriving (Eq, Generic, Show)

instance A.FromJSON RequestWithID

-- | The actual request payload.
--
-- This needs manual aeson instances, because
-- purescript and haskell don't agree on the
-- encoding for sum types.
--
-- the haskell side is changed because there is
-- more documentation for aeson.
data Request
  = GenericCommand { cmd :: !Text }
  | GetChannelMessages { channel :: !Channel
                       , before :: !(Maybe OrderedUUID) }
  deriving (Eq, Generic, Show)

instance A.FromJSON Request where
  parseJSON =
    withObject "Request" $ \o -> do
      tag <- o .: "tag"
      c <- o .: "contents"
      case (tag :: Text) of
        "GenericCommand" -> GenericCommand <$> c .: "cmd"
        "GetChannelMessages" ->
          GetChannelMessages <$> c .: "channel" <*> c .:? "before"
        _ -> fail ("Unknown Request constructor: " <> T.unpack tag)

-- | The response payload
--
-- This needs manual aeson instances, because
-- purescript and haskell don't agree on the
-- encoding for sum types.
--
-- the haskell side is changed because there is
-- more documentation for aeson.
--
-- note that messages from the IrcServer, real or pretend,
-- DO NOT carry a request id, because they are always broadcast.
data Response
  = Done { rqid :: !RequestID }
  -- ^ Generic reply for things that don't get an answer
  -- or get that answer via broadcast without request id.
  | GenericError { rqid :: !RequestID
                 , errorMsg :: !Text }
  -- ^ There was an error with request rqid
  | DecodeError { errorMsg :: !Text }
  -- ^ The request could not be decode, so we can't reply with a request id
  | GenericMessage { uuid :: OrderedUUID
                   , msg :: !Text }
  -- ^ A generic message is anything from the IRC server
  | ChannelMessage { uuid :: OrderedUUID
                   , from :: !From
                   , cmd :: !Text
                   , channels :: ![Channel]
                   , msg :: !Text }
  -- ^ A channel message is something that should be
  -- displayed in a channel.  It is from a humam
  -- sender (i.e. a prefix :nick!user@host), and it is either
  -- a PRIVMSG or NOTICE
  deriving (Eq, Generic, Show)

instance A.ToJSON Response where
  toJSON Done {..} =
    object ["tag" .= ("Done" :: Text), "contents" .= object ["rqid" .= rqid]]
  toJSON GenericError {..} =
    object
      [ "tag" .= ("GenericError" :: Text)
      , "contents" .= object ["rqid" .= rqid, "errorMsg" .= errorMsg]
      ]
  toJSON DecodeError {..} =
    object
      [ "tag" .= ("DecodeError" :: Text)
      , "contents" .= object ["errorMsg" .= errorMsg]
      ]
  toJSON GenericMessage {..} =
    object
      [ "tag" .= ("GenericMessage" :: Text)
      , "contents" .= object ["uuid" .= uuid, "msg" .= msg]
      ]
  toJSON ChannelMessage {..} =
    object
      [ "tag" .= ("ChannelMessage" :: Text)
      , "contents" .=
        object
          [ "uuid" .= uuid
          , "from" .= from
          , "cmd" .= cmd
          , "channels" .= channels
          , "msg" .= msg
          ]
      ]

-- | A request id identifies a request on a websocket
newtype RequestID =
  RequestID SC.Scientific
  deriving (Eq, Ord, Hashable, Generic, Show)

instance A.FromJSON RequestID where
  parseJSON = A.withScientific "RequestID" (pure . RequestID)

instance A.ToJSON RequestID where
  toJSON (RequestID x) = A.Number x

-- | A channel represents an IRC Channel (or nick, for private messages)
newtype Channel =
  Channel Text
  deriving (Eq, Ord, Hashable, Generic, Show)

instance A.FromJSON Channel where
  parseJSON = A.withText "Channel" (pure . Channel)

instance A.ToJSON Channel where
  toJSON (Channel cn) = A.String cn

-- | A message can be from a server or from a user
--
-- But for our channel messages, we only want messages from users
data From = From
  { nick :: !Nick
  , user :: !Text
  , host :: !Text
  } deriving (Eq, Ord, Generic, Show)

instance A.FromJSON From

instance A.ToJSON From

fromToText :: From -> Text
fromToText From {nick, user, host} = coerce nick <> "!" <> user <> "@" <> host

parseFrom :: Atto.Parser From
parseFrom =
  From <$> (Nick <$> Atto.takeWhile1 (/= '!') <* Atto.string "!") <*>
  (Atto.takeWhile1 (/= '@') <* Atto.string "@") <*>
  Atto.takeWhile1 (const True)

runParseFrom :: Text -> Maybe From
runParseFrom =
  either (const Nothing) Just . Atto.parseOnly (parseFrom <* Atto.endOfInput)

-- | A nick represents an IRC User
newtype Nick =
  Nick Text
  deriving (Eq, Ord, Hashable, Generic, Show)

instance A.FromJSON Nick where
  parseJSON = A.withText "Nick" (pure . Nick)

instance A.ToJSON Nick where
  toJSON (Nick cn) = A.String cn
