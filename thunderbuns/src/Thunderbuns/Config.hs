{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.Config where

import Data.Pool (Pool)
import Control.Lens (lens)
import Control.Lens.TH (makeClassy)
import qualified Data.Aeson as A
import Dhall (Interpret)
import System.Log.Bunyan.RIO (HasLogger(..), Logger)
import qualified Thunderbuns.Irc.Config as IC
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
import Thunderbuns.WS.Types
import Database.Persist.Sqlite (SqlBackend)
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

$(makeClassy ''LogConfig)

instance Interpret LogConfig

-- | HTTP Config
data HttpConfig = HttpConfig
  { port :: !Integer
  , rootPrefix :: !Text
  , staticDir :: !Text
  } deriving (Show, Eq, Generic)

$(makeClassy ''HttpConfig)

instance Interpret HttpConfig

-- | DB Config
data DatabaseConfig = DatabaseConfig
  { connectionURL :: !Text
  , poolSize :: Integer
  , runMigrations :: Bool
  } deriving (Show, Eq, Generic)

$(makeClassy ''DatabaseConfig)

instance Interpret DatabaseConfig

data Config = Config
  { server :: !IC.ServerConfig
  , logging :: !LogConfig
  , http :: !HttpConfig
  , database :: !DatabaseConfig
  } deriving (Show, Eq, Generic)

$(makeClassy ''Config)

instance Interpret Config

data Env = Env
  { _envConfig :: Config
  , _envLogger :: !Logger
  , _envIrcConnection :: !Connection
  , _envLogQueue :: !(MVar A.Object)
  , _envWSChan :: !(TChan Response)
  , _envDBPool :: !(Pool SqlBackend)
  }

$(makeClassy ''Env)

instance Show Env where
  show Env {_envConfig, _envLogger} =
    "{envConfig = " ++
    show _envConfig ++ "\n  , envLogger = " ++ show _envLogger ++ "}"

instance HasLogger Env where
  logger = envLogger

instance IC.HasServerConfig Config where
  serverConfig =
    lens Thunderbuns.Config.server (\s a -> s {Thunderbuns.Config.server = a})

instance IC.HasServerConfig Env where
  serverConfig = envConfig . IC.serverConfig

instance HasLogConfig Config where
  logConfig = lens logging (\s a -> s {logging = a})

instance HasLogConfig Env where
  logConfig = envConfig . logConfig

instance HasHttpConfig Config where
  httpConfig = lens http (\s a -> s {http = a})

instance HasHttpConfig Env where
  httpConfig = envConfig . httpConfig

instance HasDatabaseConfig Config where
  databaseConfig = lens database (\s a -> s {database = a})

instance HasDatabaseConfig Env where
  databaseConfig = envConfig . databaseConfig
