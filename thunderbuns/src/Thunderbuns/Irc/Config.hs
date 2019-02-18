module Thunderbuns.Irc.Config where

import Data.Text (Text)
import Dhall (Generic, Interpret)

data Server = Server
  { host :: Text
  , port :: Integer
  , ssl :: Bool
  , serverPassword :: Text
  , nick :: Text
  , nicksrvPassword :: Text
  } deriving (Generic, Eq, Show)

instance Interpret Server

