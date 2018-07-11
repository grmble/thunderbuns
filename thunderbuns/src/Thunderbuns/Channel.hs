-- | Thunderbuns Channels
--
-- You are in a maze of channels, all alike.
module Thunderbuns.Channel
  ( MonadChannel(..)
  , messages
  , messagesBefore
  , mkMsg
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Database.CQL4
import Thunderbuns.Auth.Types (Username)
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.DB.Internal (decodeError, runDB)
import Thunderbuns.Event (broadcast)
import Thunderbuns.Exception
import Thunderbuns.Logging (HasLogger, logDebug)
import Thunderbuns.OrderedUUID
  ( OrderedUUID
  , orderedUUID
  , timestamp
  , toDailyBucket
  , toUUID
  )
import Data.Time (UTCTime, getCurrentTime)
import Thunderbuns.Validate

-- | Channel abstraction
class Monad m =>
      MonadChannel m
  where
  mkCurrentTime :: m UTCTime
  mkUUID :: m UUID
  messageBuckets :: m (M.HashMap T.Text (S.Set T.Text)) -- ^ buckets by channel
  messagesInBucket :: Int -> Channel -> T.Text -> Maybe OrderedUUID -> m [Msg] -- ^ messages in the channel/bucket, before uuid, any order
  list :: m [Channel] -- ^ list of all channels
  addChannel :: V Channel -> m () -- ^ add a new channel
  addMessage :: V Msg -> m () -- ^ add a new message


-- | Number of messages returned per messages/messagesBefore
messageBatchSize :: Int
messageBatchSize = 50

-- | create msg (timeuuid!)
mkMsg :: MonadChannel m => Channel -> Username -> T.Text -> m Msg
mkMsg c u s = do
  uuid <- mkUUID
  pure $ Msg c (orderedUUID uuid) u s

messagesInBuckets :: MonadChannel m => Int -> Channel -> S.Set T.Text -> [[Msg]] -> Maybe OrderedUUID -> m [Msg]
messagesInBuckets 0 _ _ acc _ = pure $ concat acc
messagesInBuckets !n chan buckets !acc mcre =
  case S.maxView buckets of
    Nothing -> pure $ concat acc -- no more buckets
    Just (bucket, buckets') -> do
      ms <- messagesInBucket n chan bucket mcre
      messagesInBuckets (n - length ms) chan buckets' (ms : acc) Nothing

lookupSet :: T.Text -> M.HashMap T.Text (S.Set T.Text) -> S.Set T.Text
lookupSet k m =
  fromMaybe S.empty (M.lookup k m)

messages :: MonadChannel m => V Channel -> m [Msg]
messages chan' = do
  let chan = unV chan'
  buckets <- lookupSet (channelName chan) <$> messageBuckets
  messagesInBuckets messageBatchSize chan buckets [] Nothing

messagesBefore :: MonadChannel m => V Channel -> V OrderedUUID -> m [Msg]
messagesBefore chan' cre' = do
  let chan = unV chan'
  let cre = unV cre'
  let maxBucket = toDailyBucket $ timestamp cre
  allBuckets <- lookupSet (channelName chan) <$> messageBuckets
  messagesInBuckets messageBatchSize chan (S.takeWhileAntitone (<= maxBucket) allBuckets) [] (Just cre)

instance (HasDbConnection r, HasEventChannel r, HasLogger r) =>
         MonadChannel (ReaderT r (ExceptT ThunderbunsException IO)) where
  mkCurrentTime = liftIO getCurrentTime
  mkUUID =
    liftIO nextUUID >>=
    maybe (throwError $ internalError "uuid generation failed") pure
  messagesInBucket n chan bucket mcreated =
    runDB $ do
      let pre = "select json from tb.msg where channel=? and bucket=? "
      let suf = "limit " <> T.pack (show n)
      let cql = pre <> maybe "" (const "and created < ? ") mcreated <> suf
      let args =
            [TextValue (channelName chan), TextValue bucket] <>
            maybe [] (\x -> [TimeUUIDValue $ toUUID x]) mcreated
      rows <- executeQuery Quorum cql args
      for rows (extractRow extract >=> decodeError)
  messageBuckets =
    -- XXX: cache the buckets - for existing channels, they change max once per day
    runDB $ do
      let cql = "select distinct channel, bucket from tb.msg"
      rows <- executeQuery Quorum cql []
      fmap
        (M.fromListWith (<>))
        (for
           rows
           (extractRow
              ((\k v -> (k, S.singleton v)) <$> extract <*> extract)))
  list =
    runDB $ do
      let cql = "select channel from tb.channel"
      rows <- executeQuery Quorum cql []
      for rows (extractRow (Channel <$> extract))
  addChannel c =
    runDB $
    execute
      Quorum
      "insert into tb.channel (channel) values (?)"
      [TextValue (channelName (unV c))]
  addMessage vm = do
    let msg@(Msg (Channel c) k _ _) = unV vm
    runDB $
      execute
        Quorum
        "insert into tb.msg (channel, bucket, created, json) values (?,?,?,?)"
        [ TextValue c
        , TextValue (toDailyBucket $ timestamp k)
        , TimeUUIDValue (toUUID k)
        , BlobValue (BL.toStrict $ encode msg)
        ]
    logDebug "broadcasting new message"
    broadcast (unV vm)
