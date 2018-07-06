-- | Model/Controller
module Thunderfront.Controller where

import Prelude

import Bonsai (Cmd, emitMessage, emittingTask, simpleTask, unitTask)
import Bonsai.DOM (ElementId(..), affF, defaultView, elementById, focusElement, selectElementText, setLocationHash)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Bonsai.Storage (removeItem, setItem)
import Bonsai.Types (delayUntilRendered)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (State, runState, get)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (for_, elem)
import Data.Lens (assign, modifying, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign)
import Thunderbuns.WebAPI (gDecodeEvent, getChannel, getChannelBefore, getChannelByChannel, postAuth, putChannelByChannel)
import Thunderbuns.WebAPI.OrderedUUID (OrderedUUID)
import Thunderbuns.WebAPI.Types (Channel, Token(..), UserPass(..))
import Thunderbuns.WebAPI.Types as WT
import Thunderfront.EventSource (EventSource, close, newEventSource, onMessage)
import Thunderfront.Scroll (isScrolledToBottom, scrollToBottom, scrollIntoViewTop)
import Thunderfront.Sensor (prime)
import Thunderfront.Types (Model, Msg(..), activeChannel, channelName, channels, currentView, eventSource, inputModel, jwtToken, loginFormModel, messages, shouldLoadOlderSensor)

update :: Msg -> Model -> Tuple (Cmd Msg) Model
update (JwtTokenMsg x) = runState $ updateJwtToken x
update (EventSourceMsg x) = runState $ updateEventSource x
update (LoginFormMsg x) = runState $ updateLoginForm x
update (MessageInputMsg x) = runState $ updateInputForm x
update (ChannelListMsg x) = runState $ updateChannelList x
update (ActiveChannelMsg x) = runState $ updateActiveChannel x
update GetChannelBeforeMsg = runState updateChannelBefore
update (MessageMsg x) = runState $ setMessages (messageMap x)
update (MessageBeforeMsg x) = runState $ mergeMessages (messageMap x)
update (EventMsg x) = runState $ updateEvent x
update (NewMessageMsg x) = runState $ addMessage x
update (CurrentViewMsg x) = runState $ assign currentView x *> pure empty

messageMap :: Array WT.Msg -> M.Map OrderedUUID WT.Msg
messageMap ms =
  M.fromFoldable (map (\m -> Tuple (unwrap m).created m) ms)

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
  active <- use activeChannel
  case (active == msgRec.channel) of
    false -> pure empty
    true -> do
      modifying messages (M.insert msgRec.created msg)
      scrollMessagesIfAtEnd

scrollMessagesIfAtEnd :: State Model (Cmd Msg)
scrollMessagesIfAtEnd =
  pure $ emittingTask \ctx -> do
    elem <- affF $ elementById (ElementId "messages") ctx.document
    atEnd <- affF $ isScrolledToBottom elem
    when atEnd $ do
      delayUntilRendered ctx
      affF $ scrollToBottom elem

primeScrollAndFocus :: State Model (Cmd Msg)
primeScrollAndFocus =
  pure $ emittingTask \ctx -> do
    delayUntilRendered ctx
    liftEffect $ prime shouldLoadOlderSensor
    _ <- affF $ elementById (ElementId "msgInput") ctx.document >>= focusElement >>= selectElementText
    affF $ elementById (ElementId "messages") ctx.document >>= scrollToBottom

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
  assign channels cs
  c <- use activeChannel
  let c' = if elem c cs then c else (fromMaybe c $ Array.head cs)
  -- pure $ pure $ ActiveChannelMsg c'
  pure $ emittingTask \ctx -> do
    liftEffect $ do
      log ("Active channel:" <> show c)
      log ("Incoming channels:" <> show cs)
      log ("Elem?" <> show (elem c cs))
    emitMessage ctx $ ActiveChannelMsg c'

updateActiveChannel :: Channel -> State Model (Cmd Msg)
updateActiveChannel c = do
  assign activeChannel c
  s <- get
  pure $ simpleTask $ \doc -> do
    let n = view channelName c
    cs <- runReaderT (getChannelByChannel c) s
    affF $ setLocationHash ("#!" <> n) doc
    pure $ MessageMsg cs

updateChannelBefore :: State Model (Cmd Msg)
updateChannelBefore = do
  c <- use activeChannel
  oldest <- oldestUUID
  s <- get
  pure $ emittingTask $ \ctx -> do
    case oldest of
      Nothing -> pure unit
      Just created -> do
        ms <- runReaderT (getChannelBefore c created) s
        emitMessage ctx $ MessageBeforeMsg ms

oldestUUID :: State Model (Maybe OrderedUUID)
oldestUUID = do
  ms <- use messages
  pure $ _.key <$> (M.findMin ms)

setMessages :: M.Map OrderedUUID WT.Msg -> State Model (Cmd Msg)
setMessages ms = do
  assign messages ms
  primeScrollAndFocus

mergeMessages :: M.Map OrderedUUID WT.Msg -> State Model (Cmd Msg)
mergeMessages ms = do
  oldest <- oldestUUID
  modifying messages (ms <> _)
  case oldest of
    Nothing -> pure empty
    Just oldestId -> do
      pure $ emittingTask \ctx -> do
        delayUntilRendered ctx
        liftEffect $ prime shouldLoadOlderSensor
        affF $ elementById (ElementId (unwrap oldestId)) ctx.document >>= scrollIntoViewTop true


addMessage :: String -> State Model (Cmd Msg)
addMessage str = do
  c <- use activeChannel
  s <- get
  pure $ emittingTask $ \ctx -> do
    -- yes, reqBody -> channel
    runReaderT (putChannelByChannel (WT.NewMsg { msg: str} ) c) s
    -- emitMessage ctx $ ActiveChannelMsg c
    emitMessage ctx $ MessageInputMsg ""


updateInputForm :: String -> State Model (Cmd Msg)
updateInputForm s = do
  assign inputModel s
  pure empty
