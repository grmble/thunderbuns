module Main where

import Prelude

import Bonsai (BONSAI, Cmd(..), debugProgram, emitMessage, emittingTask, fullDebug, noDebug, simpleTask)
import Bonsai.DOM (DOM, ElementId(..), document, effF, locationHash, window)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Bonsai.Html (Markup, VNode, a, div_, input, li, mapMarkup, nav, render, span, text, ul, (!), (#!))
import Bonsai.Html.Attributes (cls, id_, href, style, target, typ, value)
import Bonsai.Html.Events (onClick, onClickPreventDefault, onInput, onKeyEnter)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State (State, runState, get)
import Control.Plus (empty)
import Data.Array (head)
import Data.Foldable (for_, elem)
import Data.Lens (assign, modifying, set, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Thunderbuns.WebAPI (getChannel, getChannelByChannel, postAuth, putChannelByChannel)
import Thunderbuns.WebAPI.Types (_Channel, Channel(..), Token(..), UserPass(..))
import Thunderbuns.WebAPI.Types as WT
import Thunderfront.Forms.Login (loginForm)
import Thunderfront.Types (CurrentView(..), Model, Msg(..), activeChannel, channelList, channelModel, channelName, channels, currentView, emptyModel, inputModel, jwtToken, loginFormModel, messages)
import Thunderfront.Utils (runAjax, runAjaxTask)

update
  :: forall eff
  .  Msg
  -> Model
  -> Tuple (Cmd (ajax::AJAX,dom::DOM|eff) Msg) Model
update (JwtTokenMsg x) = runState $ updateJwtToken x
update (LoginFormMsg x) = runState $ updateLoginForm x
update (MessageInputMsg x) = runState $ updateInputForm x
update (ChannelListMsg x) = runState $ updateChannelList x
update (ActiveChannelMsg x) = runState $ updateActiveChannel x
update (MessageMsg x) = runState $ updateMessages x
update (NewMessageMsg x) = runState $ addMessage x
update (CurrentViewMsg x) = runState $ assign currentView x *> pure empty

updateJwtToken
  :: forall eff
  .  Maybe String -> State Model (Cmd (|eff) Msg)
updateJwtToken jwt = do
  assign jwtToken jwt
  pure empty

updateLoginForm
  :: forall eff
  .  FormMsg -> State Model (Cmd (ajax::AJAX|eff) Msg)
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


updateChannelList
  :: forall eff
  .  Array Channel -> State Model (Cmd (|eff) Msg)
updateChannelList cs = do
  assign (channelList <<< channels) cs
  c <- use (channelList <<< activeChannel)
  -- XXX eq classes!
  pure $ pure $ ActiveChannelMsg (fromMaybe c $ head cs)

updateActiveChannel
  :: forall eff
  .  Channel -> State Model (Cmd (ajax::AJAX|eff) Msg)
updateActiveChannel c = do
  assign (channelList <<< activeChannel) c
  s <- get
  map (map MessageMsg) (runAjaxTask (getChannelByChannel (view channelName c)) s)


updateMessages
  :: forall eff
  .  Array WT.Msg -> State Model (Cmd (|eff) Msg)
updateMessages ms = do
  assign (channelModel <<< messages) ms
  pure empty

addMessage :: forall eff. String -> State Model (Cmd (ajax::AJAX|eff) Msg)
addMessage str = do
  c <- use (channelList <<< activeChannel)
  let cn = view channelName c
  s <- get
  pure $ emittingTask $ \ctx -> do
    -- yes, reqBody -> channel
    runAjax (putChannelByChannel (WT.NewMsg { msg: str} ) cn) s
    emitMessage ctx $ ActiveChannelMsg c
    emitMessage ctx $ MessageInputMsg ""


updateInputForm
  :: forall eff
  .  String -> State Model (Cmd (|eff) Msg)
updateInputForm s = do
  assign inputModel s
  pure empty


viewMain :: Model -> VNode Msg
viewMain model = do
  render $
    div_ ! id_ "bonsai-main" ! cls "pure-g" $
      case (view jwtToken model) of
        Nothing ->
          viewLoginForm model
        Just tk -> do
          viewMenu model
          viewCurrentView model

viewLoginForm :: Model -> Markup Msg
viewLoginForm model =
  div_ ! cls "pure-u-1 l-box"
    -- #! style "margin-left" "2em"
    $ mapMarkup LoginFormMsg $ loginForm model

viewMenu ::  Model -> Markup Msg
viewMenu model = do
  nav ! id_ "menu" ! cls "l-box pure-u-1 pure-menu pure-menu-horizontal pure-menu-scrollable" $ do
    span ! cls "pure-menu-heading" $ text "Thunderbuns"
    ul ! cls "pure-menu-list" $ do
      item ChannelView "Channels"
      item DebugView "Debug"
      item' "pure-menu-item" (JwtTokenMsg Nothing) "Logout"
  where
    item current str =
      item' (menuItemClasses current) (CurrentViewMsg current) str
    item' klass msg str =
      li ! cls klass $
        a ! cls "pure-menu-link" ! href "#"
          ! onClickPreventDefault msg
          $ text str
    menuItemClasses current =
      if current == (view currentView model)
          then "pure-menu-item pure-menu-item-selected"
          else "pure-menu-item"

viewCurrentView :: Model -> Markup Msg
viewCurrentView model =
  case (view currentView model) of
    ChannelView -> do
      viewChannels model
      viewActiveChannel model
    DebugView -> viewDebug model

viewChannels :: Model -> Markup Msg
viewChannels model = do
  div_ ! id_ "channels" ! cls "l-box pure-u-1-3 pure-u-md-1-6 pure-menu" $ do
    span ! cls "pure-menu-heading" $ text "Channels"
    ul ! cls "l-plainlist" $
    for_ (view (channelList <<< channels) model) $ \c -> do
      li ! cls (menuItemClasses c) $
        a ! cls "pure-menu-link" ! href "#"
          ! onClickPreventDefault (ActiveChannelMsg c)
          $ text (view channelName c)
  where
    -- XXX eq classes generated! unify with viewMenu
    menuItemClasses current =
      if (view channelName current) == (view (channelList <<< activeChannel <<< channelName) model)
          then "pure-menu-item pure-menu-item-selected"
          else "pure-menu-item"

viewActiveChannel :: Model -> Markup Msg
viewActiveChannel model =
  div_ ! id_ "content" ! cls "l-box pure-u-2-3 pure-u-md-5-6" $ do
    ul ! cls "l-plainlist l-stretch" $
      for_ (view (channelModel <<< messages) model) $ \(WT.Msg {msg})  ->
        li $ text msg
    div_ ! cls "pure-form" $
      input ! cls "pure-u-1 pure-input" ! typ "text" ! value (view inputModel model)
        ! onInput MessageInputMsg ! onKeyEnter NewMessageMsg

viewDebug :: Model -> Markup Msg
viewDebug model = do
  div_ ! cls "l-box pure-u-1" $ do
    div_ $ text "DEBUG PANE"

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
