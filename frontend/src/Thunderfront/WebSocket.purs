module Thunderfront.WebSocket where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Foreign (Foreign)

newtype WebSocket = WebSocket Foreign

derive instance newtypeWebSocket :: Newtype WebSocket _
derive instance genericWebSocket :: Generic WebSocket _
instance showWebSocket :: Show WebSocket where show = const "a WebSocket"

foreign import primitives
  :: { newWebSocket :: String -> WebSocket
     , onOpen :: Fn2 (Foreign -> Effect Unit) WebSocket Unit
     , onMessage :: Fn2 (Foreign -> Effect Unit) WebSocket Unit
     , onClose :: Fn2 (Foreign -> Effect Unit) WebSocket Unit
     , onError :: Fn2 (Foreign -> Effect Unit) WebSocket Unit
     , close :: WebSocket -> Unit
     , send :: Fn2 String WebSocket Unit
     , consoleHandler :: Foreign -> Effect Unit}

newWebSocket :: String -> Effect WebSocket
newWebSocket url =
  pure $ primitives.newWebSocket url

onOpen :: (Foreign -> Effect Unit) -> WebSocket -> Effect Unit
onOpen handler ws =
  pure $ runFn2 primitives.onOpen handler ws

onMessage :: (Foreign -> Effect Unit) -> WebSocket -> Effect Unit
onMessage handler ws =
  pure $ runFn2 primitives.onMessage handler ws

onClose :: (Foreign -> Effect Unit) -> WebSocket -> Effect Unit
onClose handler ws =
  pure $ runFn2 primitives.onClose handler ws

onError :: (Foreign -> Effect Unit) -> WebSocket -> Effect Unit
onError handler ws =
  pure $ runFn2 primitives.onError handler ws

close :: WebSocket -> Effect Unit
close ws = pure $ primitives.close ws

send :: String -> WebSocket -> Effect Unit
send s ws = pure $ runFn2 primitives.send s ws

consoleHandler :: Foreign -> Effect Unit
consoleHandler = primitives.consoleHandler
