module Thunderbuns.Server.Types where

import Thunderbuns.Logging (MonadTLogger(..), WrappedRIO(..), MonadTLogger(..), HasLogger(..))
import Servant.Server (Handler)
import Control.Monad.Reader

instance HasLogger r => MonadTLogger (ReaderT r Handler) where
  localLogger n ctx action = unwrapRIO $ localLogger n ctx (WrappedRIO action)
  logRecord pri obj msg = unwrapRIO $ logRecord pri obj msg
  getSystemTime = unwrapRIO getSystemTime

