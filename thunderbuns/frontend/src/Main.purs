module Main where

import Prelude

import Bonsai (BONSAI, Cmd, debugProgram, emitMessage, emittingTask, noDebug)
import Bonsai.DOM (DOM, ElementId(..), document, effF, locationHash, window)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Bonsai.Html (VNode, div_, li, mapMarkup, render, text, ul, (!))
import Bonsai.Html.Attributes (cls, id_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State (State, runState, get)
import Control.Plus (empty)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Thunderbuns.WebAPI (getChannel, postAuth)
import Thunderbuns.WebAPI.Types (Channel, Token(..), UserPass(..))
import Thunderfront.Forms.Login (loginForm)
import Thunderfront.Types (class HasInputModel, class HasChannelList, class HasJwtToken, class HasLoginFormModel, Model, Msg(..), channels, channelList, channelName, emptyModel, inputModel, jwtToken, loginFormModel)
import Thunderfront.Utils (runAjax)

update
  :: forall eff
  .  Msg
  -> Model
  -> Tuple (Cmd (ajax::AJAX,dom::DOM|eff) Msg) Model
update (JwtTokenMsg msg) = runState $ updateJwtToken msg
update (LoginFormMsg msg) = runState $ updateLoginForm msg
update (InputFormMsg msg) = runState $ updateInputForm msg
update (ChannelListMsg msg) = runState $ updateChannelList msg

updateJwtToken
  :: forall eff m. HasJwtToken m
  => Maybe String -> State m (Cmd (|eff) Msg)
updateJwtToken jwt = do
  assign jwtToken jwt
  pure empty

updateLoginForm
  :: forall eff m. HasLoginFormModel m => HasJwtToken m
  => FormMsg -> State m (Cmd (ajax::AJAX|eff) Msg)
updateLoginForm FormOK = do
  -- XXX: it's required -- type safe helpers for form lib?
  u <- (fromMaybe "" <<< lookup "login_username") <$> use loginFormModel
  p <- (fromMaybe "" <<< lookup "login_password") <$> use loginFormModel
  -- reset the password for the next time the form is shown
  modifying loginFormModel (M.delete "password")
  model <- get
  pure $ emittingTask $ \ctx -> do
    Token {token} <- runAjax (postAuth (UserPass {user: u, pass:p})) model
    emitMessage ctx (JwtTokenMsg $ Just token)
    let model' = set jwtToken (Just token) model
    cs <- runAjax getChannel model'
    emitMessage ctx (ChannelListMsg cs)
updateLoginForm msg = do
  modifying loginFormModel (updatePlain msg)
  pure empty


updateChannelList :: forall eff m. HasJwtToken m => HasChannelList m
  => Array Channel -> State m (Cmd (|eff) Msg)
updateChannelList cs = do
  assign (channelList <<< channels) cs
  pure empty

updateInputForm
  :: forall eff m. HasInputModel m
  => FormMsg -> State m (Cmd (|eff) Msg)
updateInputForm msg = do
  modifying inputModel (updatePlain msg)
  pure empty


viewMain :: Model -> VNode Msg
viewMain model = do
  render $
    div_ ! id_ "bonsai-main" ! cls "pure-g" $
      div_ ! cls "pure-u-1" $
        case (view jwtToken model) of
          Nothing ->
            mapMarkup LoginFormMsg $ loginForm model
          Just tk ->
            div_ $ ul $ do
              for_ (view (channelList <<< channels) model) $ \c -> do
                li $ text (view channelName c)


main :: Eff (bonsai::BONSAI, dom::DOM, exception::EXCEPTION) Unit
main = do
  hash <- effF $ window >>= document >>= locationHash
  _ <- dbgProgram (ElementId "main") update viewMain emptyModel window
  pure unit

  where
    dbgProgram =
      debugProgram (noDebug
        { timing = true
        , events = true
        })
