{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Persist.Api where

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.ByteString.D64.UUID (OrderedUUID)
import Data.Coerce (coerce)
import Data.Pool (withResource)
import Database.Persist
import Database.Persist.Sqlite
import System.Log.Bunyan.RIO
import qualified Thunderbuns.Config as C
import Thunderbuns.Persist.Gen
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Types as W
  ( Channel(..)
  , Response(..)
  , fromToText
  , runParseFrom
  )

-- type MyBackend backend = (IsSqlBackend backend, PersistStoreWrite backend)
-- NOT HasDatabaseConfig - Env is just being assembled when this is called
runCreateSqlitePool :: Bunyan r m => C.DatabaseConfig -> m C.DatabasePool
runCreateSqlitePool cfg = do
  let C.DatabaseConfig {connectionURL, poolSize, runMigrations} = cfg
  pool <-
    liftIO $
    runNoLoggingT $ createSqlitePool connectionURL (fromIntegral poolSize)
  when runMigrations $ do
    logInfo "running migrations"
    liftIO $ withResource pool $ runReaderT (runMigration migrateAll)
  pure (C.DatabasePool pool)

withDatabasePool ::
     (C.HasDatabasePool r, Bunyan r m) => ReaderT SqlBackend IO a -> m a
withDatabasePool persistAction =
  logDuration TRACE "withDatabasePool" $ do
    pool <- view (C.databasePool . C._DatabasePool)
    liftIO $ withResource pool (runReaderT persistAction)

insertResponse :: W.Response -> ReaderT SqlBackend IO ()
insertResponse resp = for_ (responseToMessage resp) (void . insert)

selectChannelBefore ::
     W.Channel -> Maybe OrderedUUID -> ReaderT SqlBackend IO [W.Response]
selectChannelBefore channel before = do
  messages <-
    case before of
      Nothing ->
        selectList
          [MessageChannel ==. coerce channel]
          [LimitTo channelBeforeLimit, Desc MessageUuid]
      Just uuid ->
        selectList
          [MessageChannel ==. coerce channel, MessageUuid <. uuid]
          [LimitTo channelBeforeLimit, Desc MessageUuid]
  pure $ foldMap (\(Entity _ x) -> messageToResponse x) messages
  where
    channelBeforeLimit = 50

messageToResponse :: Message -> [W.Response]
messageToResponse (Message uuid from cmd channel msg) =
  maybe
    []
    (\x -> [W.ChannelMessage uuid x cmd [coerce channel] msg])
    (W.runParseFrom from)

responseToMessage :: W.Response -> [Message]
responseToMessage W.ChannelMessage {..} =
  flip fmap channels $ \channel ->
    Message uuid (W.fromToText from) cmd (coerce channel) msg
responseToMessage _ = []
