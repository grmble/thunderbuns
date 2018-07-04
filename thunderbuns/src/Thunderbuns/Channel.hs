-- | Thunderbuns Channels
--
-- You are in a maze of channels, all alike.
module Thunderbuns.Channel where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Traversable
import Data.UUID (UUID, fromText, toText)
import Data.UUID.V1 (nextUUID)
import Database.CQL4
import Thunderbuns.Auth.Types (Username)
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.DB.Internal (runDB)
import Thunderbuns.Event (broadcast)
import Thunderbuns.Exception
import Thunderbuns.Logging (HasLogger, logDebug)
import Thunderbuns.Validate

-- | Channel abstraction
class Monad m =>
      MonadChannel m
  where
  list :: m [Channel] -- ^ list of all channels
  addChannel :: V Channel -> m () -- ^ add a new channel
  messages :: V Channel -> m [Msg] -- ^ messages in the channel
  messagesBefore :: V Channel -> V UUID -> m [Msg] -- ^ messages in the channel before message pk
  addMessage :: V Msg -> m () -- ^ add a new message
  mkMsg :: Channel -> Username -> T.Text -> m Msg -- ^ create msg (timeuuid!)

instance (HasDbConnection r, HasEventChannel r, HasLogger r) =>
         MonadChannel (ReaderT r (ExceptT ThunderbunsException IO)) where
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
  messages c =
    runDB $ do
      let cql = "select created, user, msg from tb.msg where channel=? limit 50"
      rows <- executeQuery Quorum cql [TextValue (channelName (unV c))]
      for
        (reverse rows)
        (extractRow
           (Msg <$> pure (unV c) <*> fmap toText extract <*> extract <*> extract))
  messagesBefore c pk =
    runDB $ do
      let cql = "select created, user, msg from tb.msg where channel=? and created<? limit 10"
      rows <- executeQuery Quorum cql [TextValue (channelName (unV c)), TimeUUIDValue (unV pk)]
      for
        (reverse rows)
        (extractRow
           (Msg <$> pure (unV c) <*> fmap toText extract <*> extract <*> extract))
  addMessage vm = do
    let Msg (Channel c) k u m = unV vm
    uu <-
      maybe
        (throwError $ internalError ("uuid can not be parsed" ++ T.unpack k))
        pure
        (fromText k)
    runDB $
      execute
        Quorum
        "insert into tb.msg (channel, created, user, msg) values (?,?,?,?)"
        [TextValue c, TimeUUIDValue uu, TextValue u, TextValue m]
    logDebug "broadcasting new message"
    broadcast (unV vm)
  mkMsg c u s =
    liftIO nextUUID >>=
    maybe
      (throwError $ internalError "uuid generation failed")
      (\uuid -> pure $ Msg c (toText uuid) u s)
