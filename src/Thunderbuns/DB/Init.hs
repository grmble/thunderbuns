module Thunderbuns.DB.Init where

import Data.Semigroup ((<>))
import Data.Foldable (for_)
import Control.Monad.Reader
import Thunderbuns.Config
import Database.CQL4


initDB :: HasDbConnection e => ReaderT e IO ()
initDB = do
  dbc <- ask >>= dbConnection
  liftIO $ runConnection' initDB' dbc

initDB' :: ConnectionIO ()
initDB' = do
  let cql =
        [ "create keyspace if not exists tb_users " <>
          "with replication = { 'class': 'SimpleStrategy', 'replication_factor': 1} " <>
          "and durable_writes = false"
        , "use tb_users"
        , "create table if not exists passwd (" <>
          "  username text, config blob, salt blob, hash blob, " <>
          "  primary key (username))"]
  for_ cql (\c -> execute One c [])
