{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config.Jwt where

import Control.Lens.TH (makeClassy)
import qualified Data.Text as T
import Dhall

-- | Options for Jwt Generation
data JwtConfig = JwtConfig
  { _secret :: T.Text
  , _lifetime :: Integer -- in seconds
  } deriving (Generic, Show)

$(makeClassy ''JwtConfig)

instance Interpret JwtConfig
