{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | WS Types
--
-- These are the types that go over the (websocket) wire.
--
module Thunderbuns.WS.Types where

import Data.Monoid ((<>))
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Hashable (Hashable)
import qualified Data.Scientific as SC
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Text as T

-- | Request defines anything that can be received from the frontend
--
-- Note that the backend will never see a request without id
data RequestWithID = RequestWithID
  { rqid :: RequestID
  , rq :: Request
  } deriving (Eq, Generic, Show)

instance A.FromJSON RequestWithID

instance A.ToJSON RequestWithID

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
  | ChannelCommand { nick :: !Nick
                   , cmd :: !Text
                   , channel :: !Channel
                   , msg :: !Text }
  deriving (Eq, Generic, Show)

instance A.FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    tag <- o.: "tag"
    c <- o.: "contents"
    case tag of
      ("GenericCommand" :: Text) -> do
        cmd <- c .: "cmd"
        pure GenericCommand{..}
      ("ChannelCommand" :: Text) -> do
        nick <- c .: "nick"
        cmd <- c .: "cmd"
        channel <- c .: "channel"
        msg <- c .: "msg"
        pure ChannelCommand{..}
      _ -> fail ("Unknown Request constructor: " <> T.unpack tag)

instance A.ToJSON Request


-- | Response defines anything that can be sent to the frontend
--
-- Since all responses can carry an optional request id,
-- again there is RequestWithID (which goes over the wire)
-- and Response which is the sumtype payload.
data ResponseWithID = ResponseWithID
  { rqid :: !(Maybe RequestID)
  , rs :: !Response
  } deriving (Eq, Generic, Show)

instance A.FromJSON ResponseWithID

instance A.ToJSON ResponseWithID

-- | The response payload
--
-- This needs manual aeson instances, because
-- purescript and haskell don't agree on the
-- encoding for sum types.
--
-- the haskell side is changed because there is
-- more documentation for aeson.
data Response
  = GenericMessage { msg :: !Text }
  | ChannelMessage { nick :: !Nick
                   , cmd :: !Text
                   , channel :: !Channel
                   , msg :: !Text }
  | ErrorMessage { errorMsg :: !Text }
  deriving (Eq, Generic, Show)

instance A.FromJSON Response where
  parseJSON = withObject "Response" $ \o -> do
    tag <- o.: "tag"
    c <- o.: "contents"
    case tag of
      ("GenericMessage" :: Text) -> do
        msg <- c .: "cmd"
        pure GenericMessage{..}
      ("ChannelMessage" :: Text) -> do
        nick <- c .: "nick"
        cmd <- c .: "cmd"
        channel <- c .: "channel"
        msg <- c .: "msg"
        pure ChannelMessage{..}
      ("ErrorMessage" :: Text) -> do
        errorMsg <- c .: "errorMsg"
        pure ErrorMessage{..}
      _ -> fail ("Unknown Response constructor: " <> T.unpack tag)

instance A.ToJSON Response where
  toJSON GenericMessage{..} = object ["tag" .= ("GenericMessage" :: Text)
                                     , "contents" .= object ["msg" .= msg]]
  toJSON ChannelMessage{..} = object ["tag" .= ("ChannelMessage" :: Text)
                                     , "contents" .= object
                                       [ "nick" .= nick
                                       , "cmd" .= cmd
                                       , "channel" .= channel
                                       , "msg" .= msg]]
  toJSON ErrorMessage{..} = object ["tag" .= ("ErrorMessage" :: Text)
                                     , "contents" .= object ["errorMsg" .= errorMsg]]

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

-- | A nick represents an IRC User
newtype Nick =
  Nick Text
  deriving (Eq, Ord, Hashable, Generic, Show)

instance A.FromJSON Nick where
  parseJSON = A.withText "Nick" (pure . Nick)

instance A.ToJSON Nick where
  toJSON (Nick cn) = A.String cn
