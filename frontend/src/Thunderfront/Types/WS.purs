-- | Types for the websocket interface to the backend
module Thunderfront.Types.WS where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Foreign (Foreign, F, readInt, readString, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericDecodeJSON, genericEncode, genericEncodeJSON)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Foreign.Generic.Types (Options)
import Foreign.Index ((!))

-- | RequestWithID defines anything that can be sent to the backend server
data RequestWithID = RequestWithID { rqid :: RequestID, rq :: Request }

derive instance genericRequestWithID :: Generic RequestWithID _
derive instance eqRequestWithID :: Eq RequestWithID
instance showRequestWithID :: Show RequestWithID where show = genericShow
instance decodeRequestWithID :: Decode RequestWithID where decode = genericDecode interfaceOptions
instance encodeRequestWithID :: Encode RequestWithID where encode = genericEncode interfaceOptions

-- | A Request without ID.
-- |
-- | This is processed by the update loop by adding a request identifier
-- | and actually sending it out
data Request
  = GenericCommand { cmd :: String }
  | ChannelCommand { nick :: Nick, cmd :: String, channel :: Channel, msg :: String }

derive instance genericRequest :: Generic Request _
derive instance eqRequest :: Eq Request
instance showRequest :: Show Request where show = genericShow
instance decodeRequest :: Decode Request where decode = genericDecode interfaceOptions
instance encodeRequest :: Encode Request where encode = genericEncode interfaceOptions


data Response
  = Done { rqid :: RequestID }
  | GenericError { rqid :: RequestID, errorMsg :: String }
  | DecodeError { errorMsg :: String }
  | GenericMessage { uuid :: String, msg :: String }
  | ChannelMessage { uuid :: String
                   , from :: From
                   , cmd :: String
                   , channels :: Array Channel
                   , msg :: String }

derive instance genericResponse :: Generic Response _
derive instance eqResponse :: Eq Response
instance showResponse :: Show Response where show = genericShow
instance decodeResponse :: Decode Response where decode = genericDecode interfaceOptions
instance encodeResponse :: Encode Response where encode = genericEncode interfaceOptions


-- | A request id identifies a request
-- |
-- | Responses carry the id of their request, the frontend
-- | tracks outstanding requests.
newtype RequestID = RequestID Int

derive instance newtypeRequestID :: Newtype RequestID _
derive instance eqRequestID :: Eq RequestID
derive instance ordRequestID :: Ord RequestID

instance showRequestID :: Show RequestID where
  show (RequestID o) = show o

instance decodeRequestID :: Decode RequestID where
  decode = map RequestID <<< readInt

instance encodeRequestID :: Encode RequestID where
  encode = unsafeToForeign

_RequestID :: Iso' RequestID Int
_RequestID = _Newtype

-- | A message can be from a server or from a user
-- |
-- | But for our channel messages, we only want messages from users
data From = From { nick :: Nick , user :: String , host :: String }

derive instance genericFrom :: Generic From _
derive instance eqFrom :: Eq From
instance showFrom :: Show From where show = genericShow
instance decodeFrom :: Decode From where decode = genericDecode interfaceOptions
instance encodeFrom :: Encode From where encode = genericEncode interfaceOptions


-- | A Channel represents an IRC Channel (or nick, for private messages)
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

-- | A nick represents an IRC user
newtype Nick = Nick String

derive instance newtypeNick :: Newtype Nick _
derive instance eqNick :: Eq Nick
derive instance ordNick :: Ord Nick

instance showNick :: Show Nick where
  show (Nick o) = show o

instance decodeNick :: Decode Nick where
  decode = map Nick <<< readString

instance encodeNick :: Encode Nick where
  encode = unsafeToForeign

_Nick :: Iso' Nick String
_Nick = _Newtype


-- | Default Generic JSON Encode/Decode Options for Haskell interop
interfaceOptions :: Options
interfaceOptions = defaultOptions { unwrapSingleConstructors = true }

-- | Decode a webservice message event
decodeMessageEvent :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> F a
decodeMessageEvent ev = do
  (ev ! "data") >>=
    readString >>=
    genericDecodeJSON interfaceOptions


encodeForSend :: forall a rep. Generic a rep => GenericEncode rep => a -> String
encodeForSend rq = genericEncodeJSON interfaceOptions rq
