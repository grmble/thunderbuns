{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config where

import Thunderbuns.Tlude
import Control.Lens.TH (makeClassy)
import qualified Data.Aeson as A
import Dhall (Interpret)
import System.Log.Bunyan.RIO (HasLogger(..), Logger)
import qualified Thunderbuns.Irc.Config as IC
import Thunderbuns.Irc.Types
import Thunderbuns.WS.Types
import UnliftIO.MVar
import UnliftIO.STM

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
  { server :: !IC.ServerConfig
  , logging :: !LogConfig
  , http :: !HttpConfig
  } deriving (Show, Eq, Generic)

instance Interpret Config

data Env = Env
  { envConfig :: Config
  , _envLogger :: !Logger
  , envIrcConnection :: !Connection
  , envLogQueue :: !(MVar A.Object)
  , envWSChan :: !(TChan Response)
  }

$(makeClassy ''Env)

instance Show Env where
  show Env {envConfig, _envLogger} =
    "{envConfig = " ++
    show envConfig ++ "\n  , envLogger = " ++ show _envLogger ++ "}"

instance HasLogger Env where
  logger = envLogger
