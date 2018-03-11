{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Config where

import Control.Lens.TH (makeClassy)
import Data.Maybe (maybe)
import qualified Data.Text.Lazy as LT
import Dhall
import System.Environment (lookupEnv)
import qualified Thunderbuns.Config.Server as SConf
import qualified Thunderbuns.Config.DB as DBConf
import Thunderbuns.Logging

-- | Config aggregates all configuration
--
-- It is read in at application start via dhall
data Config = Config
  { _server :: SConf.ServerConfig
  , _db :: DBConf.DbConfig
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
    opts = defaultInterpretOptions { fieldModifier = LT.dropWhile (== '_')}

-- | Thunderbuns Env is Config + Logger
data Env = Env
  { _envConfig :: Config
  , _envLogger :: Logger
  }

$(makeClassy ''Env)

instance HasConfig Env where config = envConfig
instance SConf.HasServerConfig Env where serverConfig = envConfig . server
instance DBConf.HasDbConfig Env where dbConfig = envConfig . db
instance HasLogger Env where loggerL = envLogger
