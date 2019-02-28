-- | Model/Controller
module Thunderfront.Controller where

import Prelude

import Bonsai (Cmd, emittingTask, unitTask)
import Bonsai.DOM (ElementId(..), affF, elementById)
import Bonsai.Forms.Model (updatePlain)
import Bonsai.Types (delayUntilRendered)
import Control.Monad.State (State, runState)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Plus (empty)
import Data.CatList as L
import Data.Foldable (for_)
import Data.Lens (assign, modifying, over, set, use, view)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Thunderfront.Scroll (isScrolledToBottom, scrollToBottom)
import Thunderfront.Sensor (prime)
import Thunderfront.Types.Model (Model, NickAndMsg(..), activeChannel, activeRequests, channelMessages, currentView, inputModel, loginFormModel, maxRequestID, maxRequestID', messages, webSocket)
import Thunderfront.Types.Msg (Msg(..))
import Thunderfront.Types.WS as WS
import Thunderfront.WebSocket (WebSocket, close, send)
import Toastr as T

-- | Convenience type for writer/state combo
-- |
-- | Since Cmd are now Monoids, they can be used in a writer
type M = WriterT (Cmd Msg) (State Model)

runM :: M Unit -> Model -> Tuple (Cmd Msg) Model
runM m model = runState (execWriterT m) model

update :: Msg -> Model -> Tuple (Cmd Msg) Model
update (WebSocketMsg x) = runM $ updateWebSocket x
update (MessageInputMsg x) = Tuple empty <<< set inputModel x
update (LoginFormMsg x) = Tuple empty <<< over loginFormModel (updatePlain x)
update (CurrentViewMsg x) = Tuple empty <<< set currentView x
update (ActiveChannelMsg x) = runM $ do
  modifying activeChannel (\current -> if current == x then Nothing else x)
  scrollMessages
update (RequestMsg x) = runM $ updateRequest x
update (ResponseMsg (WS.ResponseWithID x)) = runM $ updateResponse x.rqid x.rs

updateWebSocket :: Maybe WebSocket -> M Unit
updateWebSocket Nothing = do
  ws <- use webSocket
  assign webSocket Nothing
  tell $ unitTask $ const $ liftEffect $ for_ ws close
updateWebSocket (Just ws) = do
  assign webSocket (Just ws)

updateRequest :: WS.Request -> M Unit
updateRequest req = do
  ws <- use webSocket
  case ws of
    Nothing -> error "Can not send message to the backend" "Websocket not connected"
    Just ws' -> do
      sendRequest ws' req
      -- also: reset the input box ...
      tell $ pure (MessageInputMsg "")

updateResponse :: Maybe WS.RequestID -> WS.Response -> M Unit
updateResponse rqid (WS.GenericMessage {msg}) = do
  markDone rqid
  modifying messages (flip L.snoc msg)
  scrollMessagesIfAtEnd
updateResponse rqid (WS.ChannelMessage {from, cmd, channels, msg}) = do
  markDone rqid
  let WS.From {nick} = from
  for_ channels $ \channel -> do
    let nm = NickAndMsg { nick, msg }
    modifying channelMessages (M.insertWith (<>) channel (L.singleton nm))
  scrollMessagesIfAtEnd
updateResponse rqid (WS.ErrorMessage {errorMsg}) = do
  -- in case the request could not be parsed
  assign activeRequests S.empty
  error errorMsg "Error from Backend"

markDone :: Maybe WS.RequestID -> M Unit
markDone Nothing = pure unit
markDone (Just rqid) = modifying activeRequests $ S.delete rqid

error :: String -> String -> M Unit
error msg = tell <<< unitTask <<< const <<< liftEffect <<< T.error msg

sendRequest :: WebSocket -> WS.Request -> M Unit
sendRequest ws req = do
  modifying maxRequestID' $ \x -> x + 1
  rqid <- use maxRequestID
  modifying activeRequests $ S.insert rqid
  tell $ emittingTask $ \ctx -> do
    liftEffect $ send (WS.encodeForSend (WS.RequestWithID { rqid: rqid, rq: req })) ws

scrollMessagesIfAtEnd :: M Unit
scrollMessagesIfAtEnd =
  tell $ emittingTask \ctx -> do
    elem <- affF $ elementById (ElementId "messages") ctx.document
    atEnd <- affF $ isScrolledToBottom elem
    when atEnd $ do
      delayUntilRendered ctx
      affF $ scrollToBottom elem

scrollMessages :: M Unit
scrollMessages =
  tell $ emittingTask \ctx -> do
    delayUntilRendered ctx
    affF $ elementById (ElementId "messages") ctx.document >>= scrollToBottom
