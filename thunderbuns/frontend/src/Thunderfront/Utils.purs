-- | Utils
module Thunderfront.Utils where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.Lens (Getter, view)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Thunderbuns.WebAPI (SPParams_(..))

-- | Generated API code uses a (MonadError AjaxError)
runAjax
  :: forall eff a
  .  ReaderT (SPSettings_ SPParams_) (ExceptT AjaxError (Aff (ajax::AJAX|eff))) a
  -> SPSettings_ SPParams_
  -> Aff (ajax::AJAX|eff) a
runAjax m r = do
  x <- runExceptT $ runReaderT m r
  case x of
    Left err -> throwError (error $ errorToString err)
    Right a  -> pure a

-- XXX: move to app state
spSettings :: SPSettings_ SPParams_
spSettings =
  defaultSettings (SPParams_ { baseURL: "/" })


-- | View the focus of a `Getter`.
--
-- see https://github.com/purescript-contrib/purescript-profunctor-lenses/issues/79
-- view :: forall s t a b m. MonadAsk s m => Getter s t a b -> m a
rview :: forall s t a b m. MonadAsk s m => Getter s t a b  -> m a
rview l = do
  s <- ask
  pure $ view l s
