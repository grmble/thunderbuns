{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Thunderbuns.Config where

import Control.Lens (lens)
import Control.Lens.TH (makeClassy, makePrisms)
import qualified Data.Aeson as A
import Database.Persist.Sqlite (SqlBackend)
import Dhall (Interpret)
import System.Log.Bunyan.RIO (HasLogger(..), Logger)
import qualified Thunderbuns.Irc.Config as IC
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
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

$(makeClassy ''LogConfig)

instance Interpret LogConfig

-- | HTTP Config
data HttpConfig = HttpConfig
  { port :: !Natural
  , rootPrefix :: !Text
  , staticDir :: !Text
  , appRoot :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

$(makeClassy ''HttpConfig)

instance Interpret HttpConfig

-- | DB Config
data DatabaseConfig = DatabaseConfig
  { connectionURL :: !Text
  , poolSize :: !Natural
  , runMigrations :: !Bool
  } deriving (Show, Eq, Generic)

$(makeClassy ''DatabaseConfig)

instance Interpret DatabaseConfig

-- | All the configs - this is what is coming from Dhall
data Config = Config
  { server :: !IC.ServerConfig
  , logging :: !LogConfig
  , http :: !HttpConfig
  , database :: !DatabaseConfig
  } deriving (Show, Eq, Generic)

$(makeClassy ''Config)

instance Interpret Config

-- | Newtype for Pool SqlBackend, so we can generate the HasDatabasePool class
newtype DatabasePool =
  DatabasePool (MVar SqlBackend)

$(makeClassy ''DatabasePool)

$(makePrisms ''DatabasePool)

-- | The Thunderbuns main environment
data Env = Env
  { _envConfig :: Config
  , _envLogger :: !Logger
  , _envIrcConnection :: !IrcConnection
  , _envLogQueue :: !(MVar A.Object)
  , _envWSChan :: !(TChan Response)
  , _envDBPool :: !DatabasePool
  }

$(makeClassy ''Env)

--
--
-- write all the instances!
--
--
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

instance HasDatabasePool Env where
  databasePool = envDBPool

instance HasIrcConnection Env where
  ircConnection = envIrcConnection
