{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config.DB where

import qualified Data.Aeson.TH as ATH
import Control.Lens.TH (makeClassy)
import qualified Data.Text as T
import Dhall

-- | Options for KDF - currently only Argon2
--
-- Variant and Version are fixed
data PasswdOptions = Argon2Options
  { iterations :: Integer
  , memory :: Integer
  , parallelism :: Integer
  } deriving (Generic, Show)

$(ATH.deriveJSON ATH.defaultOptions ''PasswdOptions)

instance Interpret PasswdOptions

-- | DbConfig is the database configuration
data DbConfig = DbConfig
  { _port :: Integer
  , _host :: T.Text
  , _passwdOptions :: PasswdOptions
  } deriving (Generic, Show)

$(makeClassy ''DbConfig)

instance Interpret DbConfig
