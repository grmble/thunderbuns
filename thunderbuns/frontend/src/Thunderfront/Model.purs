-- | Model/Controller
module Thunderfront.Model where

import Prelude

import Bonsai (Cmd, emitMessage, emittingTask, simpleTask, unitTask)
import Bonsai.Debug
import Bonsai.DOM (affF, defaultView, setLocationHash)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Bonsai.Storage (removeItem, setItem)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (State, runState, get)
import Control.Plus (empty)
import Data.Array (head, snoc)
import Data.Foldable (for_, elem)
import Data.Lens (assign, modifying, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Thunderbuns.WebAPI (gDecodeEvent, getChannel, getChannelByChannel, postAuth, putChannelByChannel)
import Thunderbuns.WebAPI.Types (Channel, Token(..), UserPass(..))
import Thunderbuns.WebAPI.Types as WT
import Thunderfront.EventSource (EventSource, close, newEventSource, onMessage)
import Thunderfront.Types (Model, Msg(..), activeChannel, channelList, channelModel, channelName, channels, currentView, eventSource, inputModel, jwtToken, loginFormModel, messages)

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
  es <- use eventSource
  assign eventSource Nothing
  pure $ unitTask $ const $ liftEffect $ for_ es close
updateEventSource (Just es) = do
  assign eventSource (Just es)
  pure $ emittingTask $ \ctx -> do
    liftEffect $ do
      onMessage (\ev -> decodeEvent ev >>= ctx.emitter) es

decodeEvent :: Foreign -> Effect Msg
decodeEvent = map EventMsg <<< gDecodeEvent

updateEvent :: WT.Msg -> State Model (Cmd Msg)
updateEvent msg@(WT.Msg msgRec) = do
  active <- use (channelList <<< activeChannel)
  when (active == msgRec.channel) $
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
