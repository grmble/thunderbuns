{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Thunderbuns.Config
where

import Control.Monad (when)
import qualified Data.Maybe
import qualified Data.Text.Lazy as LT
import Dhall
import System.Environment (lookupEnv)


data Config =
  Config
    { port :: Integer
    , debug :: Bool
    }
  deriving (Generic, Show)

instance Interpret Config

readConfig :: IO Config
readConfig = do
  cf <- lookupEnv "THUNDERBUNS_CONFIG"
  input auto (Data.Maybe.maybe "./config" LT.pack cf)
