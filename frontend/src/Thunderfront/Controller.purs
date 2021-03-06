-- | Model/Controller
module Thunderfront.Controller where

import Prelude

import Bonsai (Cmd, emittingTask, unitTask)
import Bonsai.DOM (ElementId(..), affF, elementById)
import Bonsai.Forms.Model (updatePlain)
import Bonsai.Types (delayUntilRendered)
import Control.Monad.State (State, runState)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Plus ((<|>), empty)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, over, set, use)
import Data.Lens.At (at)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Thunderfront.Scroll (isScrolledToBottom, isScrolledToTop, scrollIntoViewTop, scrollToBottom)
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
update (ActiveChannelMsg x) =
  runM $ do
  modifying activeChannel (\current -> if current == x then Nothing else x)
  scrollMessages
  ws <- use webSocket
  for_ (Tuple <$> ws <*> x) $ \(Tuple ws' chan) ->
    sendRequest ws' (WS.GetChannelMessages {channel: chan, before: Nothing})
update LoadOlderMsg = runM $ updateLoadOlder
update (RequestMsg x) = runM $ updateRequest x
update (ResponseMsg x) = runM $ updateResponse x


updateWebSocket :: Maybe WebSocket -> M Unit
updateWebSocket Nothing = do
  ws <- use webSocket
  assign webSocket Nothing
  tell $ unitTask $ const $ liftEffect $ for_ ws close
updateWebSocket (Just ws) = do
  assign webSocket (Just ws)


updateRequest :: WS.Request -> M Unit
updateRequest req =
  use webSocket >>=
  maybe (error "Can not send message to the backend" "Websocket not connected") updateRequest'

  where

    updateRequest' ws = do
      sendRequest ws req
      -- also: reset the input box ...
      tell $ pure (MessageInputMsg "")

updateLoadOlder :: M Unit
updateLoadOlder =
  use webSocket >>=
  maybe (error "Can not send message to the backend" "Websocket not connected") updateLoadOlder'

  where

    updateLoadOlder' ws =
      use activeChannel >>= maybe (pure unit) (sendLoadOlder ws)

updateResponse :: WS.Response -> M Unit
updateResponse (WS.Done {rqid})= markDone rqid
updateResponse (WS.GenericError {rqid, errorMsg}) = do
  markDone rqid
  error errorMsg "Error from Backend"

updateResponse (WS.DecodeError {errorMsg}) = do
   -- we sent a request that could not be parsed
   -- we don't get the id in the error
   -- so we reset them all
   assign activeRequests S.empty
   error errorMsg "Error from Backend"
updateResponse (WS.KnownChannels channels) =
  for_ channels $ \channel ->
    modifying channelMessages $ \cm ->
      M.alter (\mm -> mm <|> Just M.empty) channel cm

updateResponse (WS.GenericMessages ms) = do
  for_ ms $ \(WS.GenericMessage {uuid, msg, timestamp}) ->
    modifying messages (M.insert uuid (Tuple msg timestamp))
  scrollMessagesAfterUpdate Nothing

updateResponse (WS.ChannelMessages ms) = do
  oldest <- use activeChannel >>= maybe (pure Nothing) oldestMessageUUID
  for_ ms $ \(WS.ChannelMessage {uuid, from, cmd, channel, msg, timestamp}) -> do
    let WS.From {nick} = from
    let nm = NickAndMsg { uuid, nick, msg, timestamp }
    modifying channelMessages $ \cm ->
      M.alter (\mm -> Just (maybe (M.singleton uuid nm) (M.insert uuid nm) mm)) channel cm
  scrollMessagesAfterUpdate oldest

markDone :: WS.RequestID -> M Unit
markDone rqid = modifying activeRequests $ S.delete rqid

error :: String -> String -> M Unit
error msg = tell <<< unitTask <<< const <<< liftEffect <<< T.error msg

sendRequest :: WebSocket -> WS.Request -> M Unit
sendRequest ws req = do
  modifying maxRequestID' $ \x -> x + 1
  rqid <- use maxRequestID
  modifying activeRequests $ S.insert rqid
  tell $ emittingTask $ \ctx -> do
    liftEffect $ send (WS.encodeForSend (WS.RequestWithID { rqid: rqid, rq: req })) ws

oldestMessageUUID :: WS.Channel -> M (Maybe String)
oldestMessageUUID channel = do
  cm <- use (channelMessages <<< at channel)
  pure (cm >>= (map _.key <<< M.findMin))

sendLoadOlder :: WebSocket -> WS.Channel -> M Unit
sendLoadOlder ws channel = do
  modifying maxRequestID' $ \x -> x + 1
  rqid <- use maxRequestID
  modifying activeRequests $ S.insert rqid
  oldest <- oldestMessageUUID channel
  let req = WS.GetChannelMessages { channel, before: oldest }
  tell $ emittingTask $ \ctx -> do
    liftEffect $ send (WS.encodeForSend (WS.RequestWithID { rqid: rqid, rq: req})) ws

scrollMessagesAfterUpdate :: Maybe String -> M Unit
scrollMessagesAfterUpdate formerOldest =
  tell $ emittingTask \ctx -> do
    elem <- affF $ elementById (ElementId "messages") ctx.document
    atStart <- affF $ isScrolledToTop elem
    atEnd <- affF $ isScrolledToBottom elem
    when atEnd $ do
      delayUntilRendered ctx
      affF $ scrollToBottom elem
    when atStart $ do
      case formerOldest of
        Nothing -> pure unit
        Just oldestUUID -> do
          delayUntilRendered ctx
          affF (elementById (ElementId oldestUUID) ctx.document >>= scrollIntoViewTop true)

scrollMessages :: M Unit
scrollMessages =
  tell $ emittingTask \ctx -> do
    delayUntilRendered ctx
    affF $ elementById (ElementId "messages") ctx.document >>= scrollToBottom
