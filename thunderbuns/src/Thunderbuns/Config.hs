{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Config where

import qualified Data.Aeson as A
import Data.Text (Text)
import Dhall (Generic, Interpret)
import qualified Thunderbuns.Irc.Config as IC
import System.Log.Bunyan (Logger)
import UnliftIO.MVar

-- | Logging Configuration
--
-- A filename of - will log to stderr.
-- For priority, the standard FATAL, ERROR, WARN, INFO, DEBUG, TRACE are supported.
data LogConfig = LogConfig
  { filename :: !FilePath
  , priority :: !Text
  } deriving (Show, Eq, Generic)

instance Interpret LogConfig

data Config = Config
  { server :: !IC.Server
  , logging :: !LogConfig
  } deriving (Show, Eq, Generic)

instance Interpret Config

data Env = Env
  { envConfig :: Config
  , envLogger :: !Logger
  , envLogQueue :: !(MVar A.Object)
  }

instance Show Env where
  show Env {..} =
    "{envConfig = " ++
    show envConfig ++ "\n  , envLogger = " ++ show envLogger ++ "}"
