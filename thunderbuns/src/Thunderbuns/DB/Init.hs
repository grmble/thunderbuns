module Thunderbuns.DB.Init where

import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Database.CQL4
import Thunderbuns.Config
import qualified Data.Text.IO as T

initDB :: (HasDbConnection e, MonadIO m) => ReaderT e m ()
initDB = do
  dbc <- ask >>= dbConnection
  liftIO $ runConnection' initDB' dbc

initDB' :: ConnectionIO ()
initDB' = do
  let cql =
        [ "create keyspace if not exists tb " <>
          "with replication = { 'class': 'SimpleStrategy', 'replication_factor': 1}"
        , "use tb"
        , "create table if not exists passwd (" <>
          "  username text, config blob, salt blob, hash blob, " <>
          "  primary key (username))"
        , "create table if not exists channel (" <>
          "  channel text, " <>
          "  primary key (channel))"
        , "create table if not exists msg (" <>
          "  channel text, " <>
          "  created timeuuid, " <>
          "  user text, " <>
          "  msg text, " <>
          "  primary key (channel, created)) " <>
          "with comment = 'partition key: channel/date combo.  query by channel and data/time_uuid' and " <>
          "  compaction = {'class': 'DateTieredCompactionStrategy'} and " <>
          "  clustering order by (created DESC)"
          -- "compaction = { 'class': 'TimeWindowCompactionStrategy', 'compaction_window_unit': hours, 'compation_window_size': 1 }
        ]
  for_ cql $ \c -> do
    liftIO (T.putStr "Executing: " *> T.putStrLn c)
    execute One c []
