{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config.DB where

import Control.Lens.TH (makeClassy)
import qualified Data.Text as T
import Dhall

-- | DbConfig is the database configuration
data DbConfig = DbConfig
  { _port :: Integer
  , _host :: T.Text
  } deriving (Generic, Show)

$(makeClassy ''DbConfig)

instance Interpret DbConfig
