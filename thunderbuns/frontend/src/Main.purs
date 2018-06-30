module Main where

import Prelude

import Bonsai (Cmd, debugProgram, emitMessage, emittingTask, noDebug, simpleTask, unitTask)
import Bonsai.Core (issueCommand)
import Bonsai.DOM (ElementId(..), affF, document, defaultView, effF, locationHash, setLocationHash, window)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Bonsai.Html (Markup, VNode, a, div_, input, li, mapMarkup, nav, render, span, text, ul, (!))
import Bonsai.Html.Attributes (cls, id_, href, style, target, typ, value)
import Bonsai.Html.Events (onClick, onClickPreventDefault, onInput, onKeyEnter)
import Bonsai.Storage (getItem, setItem, removeItem)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (State, execState, runState, get, gets)
import Control.Plus (empty)
import Data.Array (head, snoc)
import Data.Foldable (for_, elem)
import Data.Lens (assign, modifying, set, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple)
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign)
import Thunderbuns.WebAPI (gDecodeEvent, getChannel, getChannelByChannel, postAuth, putChannelByChannel)
import Thunderbuns.WebAPI.Types (_Channel, Channel(..), Token(..), UserPass(..))
import Thunderbuns.WebAPI.Types as WT
import Thunderfront.EventSource
import Thunderfront.Forms.Login (loginForm)
import Thunderfront.Types

update :: Msg -> Model -> Tuple (Cmd Msg) Model
update (JwtTokenMsg x) = runState $ updateJwtToken x
update (EventSourceMsg x) = runState $ updateEventSource x
update (LoginFormMsg x) = runState $ updateLoginForm x
update (MessageInputMsg x) = runState $ updateInputForm x
update (ChannelListMsg x) = runState $ updateChannelList x
update (ActiveChannelMsg x) = runState $ updateActiveChannel x
update (MessageMsg x) = runState $ updateMessages x
update (EventMsg x) = runState $ updateEvent x
update (NewMessageMsg x) = runState $ addMessage x
update (CurrentViewMsg x) = runState $ assign currentView x *> pure empty

updateJwtToken :: Maybe String -> State Model (Cmd Msg)
updateJwtToken Nothing = do
  assign jwtToken Nothing
  pure $ simpleTask $ \doc -> do
    affF $ defaultView doc >>= removeItem "tbToken"
    pure $ EventSourceMsg Nothing
updateJwtToken (Just jwt) = do
  assign jwtToken (Just jwt)
  model <- get
  pure $ emittingTask $ \ctx -> do
    affF (defaultView ctx.document >>= setItem "tbToken" jwt)
    cs <- runReaderT getChannel model
    emitMessage ctx $ ChannelListMsg cs
    es <- liftEffect $ newEventSource "/events" (Just jwt)
    emitMessage ctx $ EventSourceMsg (Just es)

updateEventSource :: Maybe EventSource -> State Model (Cmd Msg)
updateEventSource Nothing = do
  es <- gets (view eventSource)
  assign eventSource Nothing
  pure $ unitTask $ const $ liftEffect $ for_ es close
updateEventSource (Just es) = do
  assign eventSource (Just es)
  pure $ emittingTask $ \ctx -> do
    liftEffect $ do
      onMessage consoleHandler es
      onMessage (\ev -> decodeEvent ev >>= ctx.emitter) es

decodeEvent :: Foreign -> Effect Msg
decodeEvent = map EventMsg <<< gDecodeEvent

updateEvent :: WT.Msg -> State Model (Cmd Msg)
updateEvent msg = do
  modifying (channelModel <<< messages) (\ms -> snoc ms msg)
  pure empty

updateLoginForm :: FormMsg -> State Model (Cmd Msg)
updateLoginForm FormOK = do
  -- XXX: it's required -- type safe helpers for form lib?
  u <- (fromMaybe "" <<< lookup "login_username") <$> use loginFormModel
  p <- (fromMaybe "" <<< lookup "login_password") <$> use loginFormModel
  -- reset the password for the next time the form is shown
  modifying loginFormModel (M.delete "password")
  model <- get
  pure $ emittingTask $ \ctx -> do
    Token {token} <- runReaderT (postAuth (UserPass {user: u, pass:p})) model
    emitMessage ctx (JwtTokenMsg $ Just token)
updateLoginForm msg = do
  modifying loginFormModel (updatePlain msg)
  pure empty

updateChannelList :: Array Channel -> State Model (Cmd Msg)
updateChannelList cs = do
  assign (channelList <<< channels) cs
  c <- use (channelList <<< activeChannel)
  let c' = if elem c cs then c else (fromMaybe c $ head cs)
  pure $ pure $ ActiveChannelMsg c'

updateActiveChannel :: Channel -> State Model (Cmd Msg)
updateActiveChannel c = do
  assign (channelList <<< activeChannel) c
  s <- get
  pure $ simpleTask $ \doc -> do
    let n = view channelName c
    cs <- runReaderT (getChannelByChannel n) s
    affF $ setLocationHash ("#!" <> n) doc
    pure $ MessageMsg cs


updateMessages :: Array WT.Msg -> State Model (Cmd Msg)
updateMessages ms = do
  assign (channelModel <<< messages) ms
  pure empty

addMessage :: String -> State Model (Cmd Msg)
addMessage str = do
  c <- use (channelList <<< activeChannel)
  let cn = view channelName c
  s <- get
  pure $ emittingTask $ \ctx -> do
    -- yes, reqBody -> channel
    runReaderT (putChannelByChannel (WT.NewMsg { msg: str} ) cn) s
    -- emitMessage ctx $ ActiveChannelMsg c
    emitMessage ctx $ MessageInputMsg ""


updateInputForm :: String -> State Model (Cmd Msg)
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

viewMenu :: Model -> Markup Msg
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
    menuItemClasses current =
      if current == (view (channelList <<< activeChannel) model)
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

main :: Effect Unit
main = do
  hash <- effF $ window >>= document >>= locationHash
  log $ "Hash: " <> hash
  jwt <- effF $ window >>= getItem "tbToken"
  let model = set (channelList <<< activeChannel <<< channelName) (String.drop 2 hash) emptyModel
  prg <- dbgProgram (ElementId "main") update viewMain model window
  issueCommand prg (pure $ JwtTokenMsg jwt)
  pure unit
  where
    dbgProgram =
      debugProgram (noDebug
        { timing = true
        , events = true
        })
