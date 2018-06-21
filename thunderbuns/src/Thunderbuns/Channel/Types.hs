{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.Channel.Types where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Thunderbuns.Auth.Types (Username)
import Thunderbuns.Validate

newtype Channel = Channel
  { channelName :: T.Text
  } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''Channel)

instance DefaultValidator Channel where
  defaultValidator msg (Channel c) =
    Channel <$> symbolValidator (appmsg msg "channelName") c

data Msg = Msg
  { channel :: Channel
  , pk :: T.Text
  , user :: Username
  , msg :: T.Text
  } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''Msg)

instance DefaultValidator Msg where
  defaultValidator s (Msg c k u m) =
    Msg <$> defaultValidator (appmsg s "channel") c <*>
    uuidValidator (appmsg s "created") k <*>
    symbolValidator (appmsg s "user") u <*>
    singleLineValidator (appmsg s "msg") m

newtype NewMsg = NewMsg
  { msg :: T.Text } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''NewMsg)
