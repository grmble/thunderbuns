{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config.Server where

import Control.Lens.TH (makeClassy)
import qualified Data.Text as T
import Dhall

-- | ServerConfig has the configuration for the server part
data ServerConfig = ServerConfig
  { _port :: Integer
  , _staticRoot :: T.Text
  } deriving (Generic, Show)

$(makeClassy ''ServerConfig)

instance Interpret ServerConfig
