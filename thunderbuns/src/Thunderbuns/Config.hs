{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Config where

import qualified Data.Aeson as A
import Data.Text (Text)
import Dhall (Generic, Interpret)
import qualified Thunderbuns.Irc.Config as IC
import Thunderbuns.Irc.Types
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

-- | HTTP Config
data HttpConfig = HttpConfig
  { port :: !Integer
  , staticDir :: !Text
  } deriving (Show, Eq, Generic)

instance Interpret HttpConfig

data Config = Config
  { server :: !IC.Server
  , logging :: !LogConfig
  , http :: !HttpConfig
  } deriving (Show, Eq, Generic)

instance Interpret Config

data Env = Env
  { envConfig :: Config
  , envLogger :: !Logger
  , envConnection :: !Connection
  , envLogQueue :: !(MVar A.Object)
  }

instance Show Env where
  show Env {..} =
    "{envConfig = " ++
    show envConfig ++ "\n  , envLogger = " ++ show envLogger ++ "}"
