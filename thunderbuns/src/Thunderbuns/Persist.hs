{-# LANGUAGE RecordWildCards #-}

module Thunderbuns.Persist where

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader
import Data.ByteString.D64.UUID (OrderedUUID)
import Data.Coerce (coerce)
import Data.Pool (Pool, withResource)
import Database.Persist
import Database.Persist.Sqlite
import System.Log.Bunyan
import Thunderbuns.Persist.Gen
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Types as W

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
  withResource pool (runReaderT (insertResponse response))

insertResponse :: W.Response -> ReaderT SqlBackend IO ()
insertResponse resp = for_ (responseToMessage resp) (void . insert)


runSelectChannelBefore :: W.Channel -> Maybe OrderedUUID -> Pool SqlBackend -> IO [W.Response]
runSelectChannelBefore channel before pool =
  withResource pool $ runReaderT (selectChannelBefore channel before)

selectChannelBefore ::
     W.Channel -> Maybe OrderedUUID -> ReaderT SqlBackend IO [W.Response]
selectChannelBefore channel before = do
  messages <-
    case before of
      Nothing ->
        selectList
          [MessageChannel ==. coerce channel]
          [LimitTo 10, Desc MessageUuid]
      Just uuid ->
        selectList
          [MessageChannel ==. coerce channel, MessageUuid <. uuid]
          [LimitTo 10, Desc MessageUuid]
  pure $ foldMap (\(Entity _ x) -> messageToResponse x) messages

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
