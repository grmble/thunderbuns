-- | Event handling - plans
--
-- Channel related events for the frontend
--
-- XXX document event handling
module Thunderbuns.Event where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Reader
import Thunderbuns.Channel.Types (Channel(..), Msg(..))
import Thunderbuns.Config (HasEventChannel(..), HasDbConnection(..))
import Thunderbuns.Exception (ThunderbunsException)
import Thunderbuns.Logging (HasLogger(..))
import UnliftIO.STM (atomically, writeTChan)
import Thunderbuns.DB.Internal (runDB)
import Thunderbuns.OrderedUUID (OrderedUUID, orderedUUID, toUUID)
import Thunderbuns.Validate (V, unV)
import Database.CQL4
import Data.Traversable (for)

class Monad m =>
      MonadEvent m
  where
  broadcast :: Msg -> m ()
  eventsSince :: V OrderedUUID -> m [Msg]

instance (HasLogger r, HasEventChannel r, HasDbConnection r) =>
         MonadEvent (ReaderT r (ExceptT ThunderbunsException IO)) where
  broadcast msg = do
    chan <- asks (view eventBroadcast)
    liftIO $ atomically $ writeTChan chan msg
  eventsSince uuid =
    pure []
    {--
    does not work with msg table - can only query with channel

    need event table ...

    runDB $ do
      let cql = "select channel, created, user, msg from tb.msg where created>?"
      rows <- executeQuery One cql [TimeUUIDValue (toUUID $ unV uuid)]
      for
        (reverse rows)
        (extractRow
           (Msg <$> fmap Channel extract <*> fmap orderedUUID extract <*> extract <*> extract))
      --}
