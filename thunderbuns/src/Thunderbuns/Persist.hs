{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Persist where

import Control.Monad.Logger (MonadLogger, NoLoggingT, runNoLoggingT)
import Control.Monad.Reader
import Data.ByteString.D64.UUID
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Data.Pool
import Data.Pool (Pool)
import Data.UUID.V1 (nextUUID)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Log.Bunyan
import Thunderbuns.Persist.Gen
import Thunderbuns.Persist.Types
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Types as W
import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Exception (bracket)
import UnliftIO.Resource (ResourceT, runResourceT)

-- type MyBackend backend = (IsSqlBackend backend, PersistStoreWrite backend)
runCreateSqlitePool :: Text -> Int -> Bool -> Logger -> IO (Pool SqlBackend)
runCreateSqlitePool url size runmig lg = do
  pool <- runNoLoggingT $ createSqlitePool url size
  when runmig $ do
    logInfo "running migrations" lg
    withResource pool $ runReaderT (runMigration migrateAll)
  pure pool

runInsertResponse :: Logger -> W.Response -> Pool SqlBackend -> IO ()
runInsertResponse lg response pool =
  flip logDuration lg $
  const $
  withResource pool $ \backend -> runReaderT (insertResponse response) backend

insertResponse :: W.Response -> ReaderT SqlBackend IO ()
insertResponse W.ChannelMessage {uuid, from, cmd, channels, msg} =
  for_ channels $ \channel -> do
    let m = Message uuid (W.fromToText from) cmd (coerce channel) msg
    void $ insert m
insertResponse _ = pure ()
