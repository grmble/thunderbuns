module Thunderbuns.WebAPI.Types where

import Prelude

import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Foreign (readString, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)

-- | OrderedUUID is just a string newtype
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

-- | And channel as well - minimal json
newtype Channel = Channel String


derive instance newtypeChannel :: Newtype Channel _
derive instance eqChannel :: Eq Channel
derive instance ordChannel :: Ord Channel

instance showChannel :: Show Channel where
  show (Channel o) = show o

instance decodeChannel :: Decode Channel where
  decode = map Channel <<< readString

instance encodeChannel :: Encode Channel where
  encode = unsafeToForeign

_Channel :: Iso' Channel String
_Channel = _Newtype

