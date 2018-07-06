module Thunderbuns.WebAPI.OrderedUUID where

import Prelude

import Data.Newtype (class Newtype)
import Foreign (readString, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)

newtype OrderedUUID = OrderedUUID String

derive instance newtypeOrderedUUID :: Newtype OrderedUUID _
derive instance eqOrderUUID :: Eq OrderedUUID
derive instance ordOrderUUID :: Ord OrderedUUID

instance showOrderedUUID :: Show OrderedUUID where
  show (OrderedUUID o) = show o

instance decodeOrderedUUID :: Decode OrderedUUID where
  decode = map OrderedUUID <<< readString

instance encodeOrderedUUID :: Encode OrderedUUID where
  encode = unsafeToForeign

