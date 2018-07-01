module Thunderfront.EventSource where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Foreign (Foreign)

newtype EventSource = EventSource Foreign

derive instance newtypeEventSource :: Newtype EventSource _
derive instance genericEventSource :: Generic EventSource _
instance showEventSource :: Show EventSource where show = const "an EventSource"

foreign import primitives
  :: { newEventSource :: Fn2 String String EventSource
     , onMessage :: Fn2 (Foreign -> Effect Unit) EventSource Unit
     , close :: EventSource -> Unit
     , consoleHandler :: Foreign -> Effect Unit}

newEventSource :: String -> Maybe String-> Effect EventSource
newEventSource url jwt =
  pure $ runFn2 primitives.newEventSource url (fromMaybe "" jwt)

onMessage :: (Foreign -> Effect Unit) -> EventSource -> Effect Unit
onMessage handler es =
  pure $ runFn2 primitives.onMessage handler es

close :: EventSource -> Effect Unit
close es = pure $ primitives.close es

consoleHandler :: Foreign -> Effect Unit
consoleHandler = primitives.consoleHandler
