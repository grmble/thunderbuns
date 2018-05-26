{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config where

import Control.Lens (Getter, view)
import Control.Lens.TH (makeClassy)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (maybe)
import qualified Data.Text.Lazy as LT
import Database.CQL4 (Connection)
import Dhall
import System.Environment (lookupEnv)
import qualified Thunderbuns.Config.DB as DBConf
import qualified Thunderbuns.Config.Server as SConf
import qualified Thunderbuns.Config.Jwt as JConf
import Thunderbuns.Logging
import UnliftIO.STM (TMVar, atomically, readTMVar)

-- | Config aggregates all configuration
--
-- It is read in at application start via dhall
data Config = Config
  { _server :: SConf.ServerConfig
  , _db :: DBConf.DbConfig
  , _jwt :: JConf.JwtConfig
  , _debug :: Bool
  } deriving (Generic, Show)

$(makeClassy ''Config)

instance Interpret Config

instance DBConf.HasDbConfig Config where
  dbConfig = db

instance SConf.HasServerConfig Config where
  serverConfig = server

-- | Read the config via Dhall
readConfig :: IO Config
readConfig = do
  cf <- lookupEnv "THUNDERBUNS_CONFIG"
  input (autoWith opts) (Data.Maybe.maybe "./config" LT.pack cf)
  where
    opts = defaultInterpretOptions {fieldModifier = LT.dropWhile (== '_')}

-- | Thunderbuns Env is Config + Logger + TVars
data Env = Env
  { _envConfig :: Config
  , _envLogger :: Logger
  , envDBConn :: TMVar Connection
  }

$(makeClassy ''Env)

instance HasConfig Env where
  config = envConfig

instance SConf.HasServerConfig Env where
  serverConfig = envConfig . server

instance DBConf.HasDbConfig Env where
  dbConfig = envConfig . db

instance JConf.HasJwtConfig Env where
  jwtConfig = envConfig . jwt

instance HasLogger Env where
  loggerL = envLogger

-- | Lens access for db connecctions
--
-- access needs to happen in STM (or IO ..)
class HasDbConnection a where
  dbConnectionL :: Getter a (TMVar Connection)

instance HasDbConnection Env
  -- yeah, it's a full lens, but it can only be viewed ... or can it?
                                                                      where
  dbConnectionL k e = fmap (\x -> e {envDBConn = x}) (k (envDBConn e))

-- | Read the global db connection from the Environment
dbConnection :: (HasDbConnection e, MonadIO m) => e -> m Connection
dbConnection e = atomically $ readTMVar $ view dbConnectionL e
