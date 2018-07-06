{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.Channel.Types where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Thunderbuns.Auth.Types (Username)
import Thunderbuns.OrderedUUID (OrderedUUID)
import Thunderbuns.Validate
import Web.HttpApiData

newtype Channel = Channel
  { channelName :: T.Text
  } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''Channel)

instance DefaultValidator Channel where
  defaultValidator msg (Channel c) =
    Channel <$> symbolValidator (appmsg msg "channelName") c

instance FromHttpApiData Channel where
  parseQueryParam = fmap Channel . parseQueryParam

instance ToHttpApiData Channel where
  toQueryParam = toQueryParam . channelName

data Msg = Msg
  { channel :: Channel
  , created :: OrderedUUID
  , user :: Username
  , msg :: T.Text
  } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''Msg)

instance DefaultValidator Msg where
  defaultValidator s (Msg c t u m) =
    Msg <$> defaultValidator (appmsg s "channel") c <*>
    uuidValidator (appmsg s "created") t <*>
    symbolValidator (appmsg s "user") u <*>
    singleLineValidator (appmsg s "msg") m

newtype NewMsg = NewMsg
  { msg :: T.Text
  } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''NewMsg)
