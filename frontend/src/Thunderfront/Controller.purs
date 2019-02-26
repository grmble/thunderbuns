-- | Model/Controller
module Thunderfront.Controller where

import Prelude

import Bonsai (Cmd, emitMessage, emittingTask, unitTask)
import Bonsai.DOM (effF)
import Bonsai.Forms.Model (FormMsg(..), lookup, updatePlain)
import Control.Monad.State (State, runState, get)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Lens (assign, modifying, use)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.Tuple (Tuple)
import Effect.Class (liftEffect)
import Thunderfront.Types.Model (Model, activeRequests, currentView, inputModel, loginFormModel, maxRequestID, maxRequestID', messages, webSocket)
import Thunderfront.Types.Msg (Msg(..))
import Thunderfront.Types.WS as WS
import Thunderfront.WebSocket (WebSocket, close, consoleHandler, newWebSocket, onClose, onError, onMessage, send)
import Toastr as T

update :: Msg -> Model -> Tuple (Cmd Msg) Model
update (CurrentViewMsg x) = runState $ assign currentView x *> pure empty
update (MessageInputMsg x) = runState $ updateInputForm x
update (LoginFormMsg x) = runState $ updateLoginForm x
update (WebSocketMsg x) = runState $ updateWebSocket x
update (RequestMsg x) = runState $ updateRequest x
update (ResponseMsg (WS.ResponseWithID x)) = runState $ updateResponse x.rqid x.rs

updateInputForm :: String -> State Model (Cmd Msg)
updateInputForm s = do
  assign inputModel s
  pure empty

{--
    ws <- liftEffect $ do
      ws <- newWebSocket ""
      onClose (const $ ctx.emitter (WebSocketMsg Nothing)) ws
      onMessage (\ev -> effF (ResponseMsg <$> WS.decodeMessageEvent ev) >>= ctx.emitter) ws
      onError consoleHandler ws
      pure ws
 --}
updateLoginForm :: FormMsg -> State Model (Cmd Msg)
updateLoginForm msg = do
  modifying loginFormModel (updatePlain msg)
  pure empty

updateWebSocket :: Maybe WebSocket -> State Model (Cmd Msg)
updateWebSocket Nothing = do
  ws <- use webSocket
  assign webSocket Nothing
  pure $ unitTask $ const $ liftEffect $ for_ ws close
updateWebSocket (Just ws) = do
  assign webSocket (Just ws)
  pure empty

updateRequest :: WS.Request -> State Model (Cmd Msg)
updateRequest req = do
  ws <- use webSocket
  case ws of
    Nothing ->
      error "Can not send message to the backend" "Websocket not connected"
    Just ws' -> do
      cmd <- sendRequest ws' req
      -- also: reset the input box ...
      pure (cmd <> pure (MessageInputMsg ""))

updateResponse :: Maybe WS.RequestID -> WS.Response -> State Model (Cmd Msg)
updateResponse rqid (WS.GenericMessage {msg}) = do
  modifying messages (flip Array.snoc msg)
  markDone rqid
  pure empty
updateResponse _ _ = pure empty

markDone :: Maybe WS.RequestID -> State Model Unit
markDone Nothing = pure unit
markDone (Just rqid) = modifying activeRequests $ S.delete rqid

error :: String -> String -> State Model (Cmd Msg)
error msg = pure <<< unitTask <<< const <<< liftEffect <<< T.error msg

sendRequest :: WebSocket -> WS.Request -> State Model (Cmd Msg)
sendRequest ws req = do
  modifying maxRequestID' $ \x -> x + 1
  rqid <- use maxRequestID
  modifying activeRequests $ S.insert rqid
  pure $ emittingTask \ctx -> do
    liftEffect $ send (WS.encodeForSend (WS.RequestWithID { rqid: rqid, rq: req })) ws
