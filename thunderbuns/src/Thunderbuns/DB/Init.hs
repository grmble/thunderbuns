module Thunderbuns.DB.Init where

import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import qualified Data.Text.IO as T
import Database.CQL4
import Thunderbuns.Config

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
          "  primary key (username)) " <>
          "with compact storage"
        , "create table if not exists event (" <> "  bucket text, " <>
          "  created timeuuid, " <>
          "  json blob, " <>
          "  primary key (bucket, created)) " <>
          "with comment = 'partition key: date.  query by bucket' and " <>
          -- "  compaction = { 'class': 'TimeWindowCompactionStrategy', 'compaction_window_unit': hours, 'compaction_window_size': 1 } and " <>
          "  compaction = { 'class': 'DateTieredCompactionStrategy' } and " <>
          " clustering order by (created DESC) and " <>
          " compact storage"
        , "create table if not exists channel (" <> "  channel text, " <>
          "  primary key (channel))" -- can't use compact storage
        , "create table if not exists msg (" <> "  channel text, " <>
          "  bucket text, " <>
          "  created timeuuid, " <>
          "  json blob, " <>
          "  primary key ((channel, bucket), created)) " <>
          "with comment = 'partition key: channel/date combo.  query by channel and data/time_uuid' and " <>
          "  compaction = {'class': 'LeveledCompactionStrategy'} and " <>
          "  clustering order by (created DESC) and " <>
          "  compact storage"
        ]
  for_ cql $ \c -> do
    liftIO (T.putStr "Executing: " *> T.putStrLn c)
    execute One c []
