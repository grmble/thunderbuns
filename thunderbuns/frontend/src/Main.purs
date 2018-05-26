module Main where

import Prelude

import Bonsai (BONSAI, Cmd, debugProgram, noDebug, simpleTask)
import Bonsai.DOM (DOM, ElementId(..), document, effF, locationHash, window)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Bonsai.Html (VNode, div_, mapMarkup, render, text, (!))
import Bonsai.Html.Attributes (cls, id_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State (State, runState)
import Control.Plus (empty)
import Data.Lens (assign, modifying, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Thunderbuns.WebAPI (postAuth)
import Thunderbuns.WebAPI.Types (Token(..), UserPass(..))
import Thunderfront.Forms.Login (loginForm)
import Thunderfront.Utils (runAjax, spSettings)
import Thunderfront.Types (class HasInputModel, class HasJwtToken, class HasLoginFormModel, Model, Msg(..), emptyModel, inputModel, jwtToken, loginFormModel)

update
  :: forall eff
  .  Msg
  -> Model
  -> Tuple (Cmd (ajax::AJAX,dom::DOM|eff) Msg) Model
update (JwtTokenMsg msg) = runState $ updateJwtToken msg
update (LoginFormMsg msg) = runState $ updateLoginForm msg
update (InputFormMsg msg) = runState $ updateInputForm msg

updateJwtToken
  :: forall eff m. HasJwtToken m
  => Maybe String -> State m (Cmd (|eff) Msg)
updateJwtToken jwt = do
  assign jwtToken jwt
  pure empty

updateLoginForm
  :: forall eff m. HasLoginFormModel m
  => FormMsg -> State m (Cmd (ajax::AJAX|eff) Msg)
updateLoginForm FormOK = do
  -- XXX: it's required -- type safe helpers for form lib?
  u <- (fromMaybe "" <<< lookup "login_username") <$> use loginFormModel
  p <- (fromMaybe "" <<< lookup "login_password") <$> use loginFormModel
  -- reset the password for the next time the form is shown
  modifying loginFormModel (M.delete "password")
  pure $ simpleTask $ \_ -> do
    Token {token} <- runAjax (postAuth (UserPass { user: u, pass: p })) spSettings
    pure $ JwtTokenMsg $ Just token
updateLoginForm msg = do
  modifying loginFormModel (updatePlain msg)
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
            div_ $ text "blubb"


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
