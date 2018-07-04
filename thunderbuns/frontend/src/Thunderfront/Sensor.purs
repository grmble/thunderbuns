-- ! Sensor are event handlers for bulk events
--
-- They handle the flood of messages by weeding
-- out events by a predicate.
--
-- They have to be primed before firing, and
-- after firing they have to be primed again.
--
-- They don't follow the usual Bonsai model
-- on being defined by the view. Instead a
-- js event handler is defined on the element.
-- This means the element has to be in a
-- keyedNode so the element will not be reused by
-- other content.
--
-- They emitted messages can't be mapped either.
module Thunderfront.Sensor
  ( Sensor
  , sensor
  , differentialSensor
  , prime
  ) where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

foreign import primitives
  :: { sensor :: (Foreign -> Effect Boolean) -> Sensor
     , differentialSensor :: (Foreign -> Effect Boolean) -> Sensor
     , prime :: Sensor -> Effect Unit
     }

-- | Sensors deal with high-volume events, e.g. scroll or mouseMove
--
-- A sensor has to to primed.  If it is primed and it receives an event
-- that is categorized as true by it's predicate, it will fire
-- (= it will return true).
--
-- A DifferntialSensor will only fire if the predicate turns from false to
-- true.
--
-- After a sensor fires, it has to be primed again, otherwise it can't fire.
--
type Sensor
  = { primed :: Boolean
    , filter :: Foreign -> Effect Boolean
    }

sensor :: (Foreign -> Effect Boolean) -> Sensor
sensor = primitives.sensor

differentialSensor :: (Foreign -> Effect Boolean) -> Sensor
differentialSensor = primitives.differentialSensor

prime :: Sensor -> Effect Unit
prime = primitives.prime
