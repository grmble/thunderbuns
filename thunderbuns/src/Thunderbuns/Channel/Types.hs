{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Channel.Types where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Thunderbuns.Validate

newtype Channel = Channel
  { channelName :: T.Text
  } deriving (Eq, Ord, Generic, Show)

$(deriveJSON defaultOptions ''Channel)

instance DefaultValidator Channel where
  defaultValidator msg (Channel c) =
    Channel <$> symbolValidator (appmsg msg "channelName") c
