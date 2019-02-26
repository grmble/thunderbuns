-- | Msg types for the Thunderbuns frontend
module Thunderfront.Types.Msg where

import Prelude

import Bonsai.Forms (FormMsg)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Thunderfront.WebSocket (WebSocket)
import Thunderfront.Types.Model as MT
import Thunderfront.Types.WS as WS


-- | Bonsai Messages
-- |
-- | * WebSocketMsg: for storing the websocket in the model.
-- |   When a new websocket is created, it is stored in the model
-- |   (with all event listeners set up).  When the current websocket
-- |   is closed, it is removed from the model.
-- |   The websocket is used to send messages to the backend.
-- | * MessageInputMsg: Message input handling (= text input for chat messages)
-- | * LoginFormMsg: Form handling messages for the login form
-- | * CurrentViewMsg: Handles the state for view switching
-- | * ResponseMsg: incoming responses from the websocket
-- | * RequestMsg: outgoing requests to the websocket.
-- |   The view code can not update the model, but a request id
-- |   has to be generated.  The view code emits a request msg,
-- |   and the update function generates the id and sends the request out.
data Msg
  = WebSocketMsg (Maybe WebSocket)
  | MessageInputMsg String
  | LoginFormMsg FormMsg
  | CurrentViewMsg MT.CurrentView
  | RequestMsg WS.Request
  | ResponseMsg WS.ResponseWithID

-- XXX generic instance for FormMsg ...
msgShow :: Msg -> String
msgShow (WebSocketMsg _) = "WebSocketMsg"
msgShow (LoginFormMsg _) = "LoginFormMsg"
msgShow (MessageInputMsg x) = "MessageInputMsg (" <> show x <> ")"
msgShow (CurrentViewMsg x) = "CurrentViewMsg (" <> show x <> ")"
msgShow (RequestMsg x) = "RequestMsg (" <> show x <> ")"
msgShow (ResponseMsg x) = "ResponseMsg (" <> show x <> ")"

derive instance genericMsg :: Generic Msg _
instance showMsg :: Show Msg where show = msgShow
