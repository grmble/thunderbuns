-- | Event handling - plans
--
-- Channel related events for the frontend
--
-- XXX document event handling
module Thunderbuns.Event where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Reader
import Thunderbuns.Channel.Types (Msg(..))
import Thunderbuns.Config (HasEventChannel(..), HasDbConnection(..))
import Thunderbuns.Exception (ThunderbunsException)
import Thunderbuns.Logging (HasLogger(..))
import UnliftIO.STM (atomically, writeTChan)
import Thunderbuns.DB.Internal (decodeError, runDB)
import Thunderbuns.OrderedUUID (OrderedUUID, toDailyBucket, timestamp, toUUID)
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
    runDB $ do
      -- XXX: bucket awareness!
      let cql = "select json from tb.event where bucket=? and created>?"
      rows <- executeQuery One cql [TextValue (toDailyBucket $ timestamp $ unV uuid), TimeUUIDValue (toUUID $ unV uuid)]
      for (reverse rows) (extractRow extract >=> decodeError)
