-- | Event handling - plans
--
-- Channel related events for the frontend
--
-- XXX document event handling
module Thunderbuns.Event where

import Control.Lens (view)
import Control.Monad.Reader
import Control.Monad.Except
import Thunderbuns.Channel.Types (Msg)
import Thunderbuns.Config (HasEventChannel(..))
import Thunderbuns.Exception (ThunderbunsException)
import UnliftIO.STM (atomically, writeTChan)

class Monad m => MonadEvent m where
  broadcast :: Msg -> m ()

instance HasEventChannel r => MonadEvent (ReaderT r (ExceptT ThunderbunsException IO)) where
  broadcast msg = do
    chan <- asks (view eventBroadcast)
    liftIO $ atomically $ writeTChan chan msg
