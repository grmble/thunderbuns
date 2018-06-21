-- | Utils
module Thunderfront.Utils where

import Prelude

import Bonsai (Cmd, simpleTask)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (State)
import Data.Maybe (fromMaybe)
import Data.Either (Either(..))
import Data.Lens (Getter, view)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Thunderbuns.WebAPI (SPParams_(..))
import Thunderfront.Types (Model, jwtToken)

-- | Translate errors and carry authorization header
--
-- Generated API code uses a (MonadError AjaxError)
runAjax
  :: forall eff a
  .  ReaderT (SPSettings_ SPParams_) (ExceptT AjaxError (Aff (ajax::AJAX|eff))) a
  -> Model
  -> Aff (ajax::AJAX|eff) a
runAjax m r = do
  x <- runExceptT $ runReaderT m (spSettings r)
  case x of
    Left err -> throwError (error $ errorToString err)
    Right a  -> pure a

-- | Run ajax as a simple task
runAjaxTask
  :: forall eff a
  .  ReaderT (SPSettings_ SPParams_) (ExceptT AjaxError (Aff (ajax::AJAX|eff))) a
  -> Model
  -> State Model (Cmd (ajax::AJAX|eff) a)
runAjaxTask m r = pure $ simpleTask $ const $ runAjax m r


spSettings :: Model -> SPSettings_ SPParams_
spSettings r =
  let jwt = view jwtToken r
      auth = "Bearer " <> fromMaybe "" jwt
  in  defaultSettings (SPParams_ { baseURL: "/", authorization: auth })


-- | View the focus of a `Getter`.
--
-- see https://github.com/purescript-contrib/purescript-profunctor-lenses/issues/79
-- view :: forall s t a b m. MonadAsk s m => Getter s t a b -> m a
rview :: forall s t a b m. MonadAsk s m => Getter s t a b  -> m a
rview l = do
  s <- ask
  pure $ view l s
