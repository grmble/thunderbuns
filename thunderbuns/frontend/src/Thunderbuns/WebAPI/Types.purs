-- File auto generated by purescript-bridge! --
module Thunderbuns.WebAPI.Types where

import Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (String)
import Thunderbuns.WebAPI.OrderedUUID (OrderedUUID)

import Prelude

newtype Channel =
    Channel {
      channelName :: String
    }

derive instance genericChannel :: Generic Channel _
derive instance newtypeChannel :: Newtype Channel _

--------------------------------------------------------------------------------
_Channel :: Iso' Channel { channelName :: String}
_Channel = _Newtype

--------------------------------------------------------------------------------
newtype Msg =
    Msg {
      channel :: Channel
    , created :: OrderedUUID
    , user :: String
    , msg :: String
    }

derive instance genericMsg :: Generic Msg _
derive instance newtypeMsg :: Newtype Msg _

--------------------------------------------------------------------------------
_Msg :: Iso' Msg { channel :: Channel, created :: OrderedUUID, user :: String, msg :: String}
_Msg = _Newtype

--------------------------------------------------------------------------------
newtype NewMsg =
    NewMsg {
      msg :: String
    }

derive instance genericNewMsg :: Generic NewMsg _
derive instance newtypeNewMsg :: Newtype NewMsg _

--------------------------------------------------------------------------------
_NewMsg :: Iso' NewMsg { msg :: String}
_NewMsg = _Newtype

--------------------------------------------------------------------------------
data Priority =
    FATAL
  | ERROR
  | WARN
  | INFO
  | DEBUG
  | TRACE

derive instance genericPriority :: Generic Priority _

--------------------------------------------------------------------------------
_FATAL :: Prism' Priority Unit
_FATAL = prism' (\_ -> FATAL) f
  where
    f FATAL = Just unit
    f _ = Nothing

_ERROR :: Prism' Priority Unit
_ERROR = prism' (\_ -> ERROR) f
  where
    f ERROR = Just unit
    f _ = Nothing

_WARN :: Prism' Priority Unit
_WARN = prism' (\_ -> WARN) f
  where
    f WARN = Just unit
    f _ = Nothing

_INFO :: Prism' Priority Unit
_INFO = prism' (\_ -> INFO) f
  where
    f INFO = Just unit
    f _ = Nothing

_DEBUG :: Prism' Priority Unit
_DEBUG = prism' (\_ -> DEBUG) f
  where
    f DEBUG = Just unit
    f _ = Nothing

_TRACE :: Prism' Priority Unit
_TRACE = prism' (\_ -> TRACE) f
  where
    f TRACE = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
newtype UserPass =
    UserPass {
      user :: String
    , pass :: String
    }

derive instance genericUserPass :: Generic UserPass _
derive instance newtypeUserPass :: Newtype UserPass _

--------------------------------------------------------------------------------
_UserPass :: Iso' UserPass { user :: String, pass :: String}
_UserPass = _Newtype

--------------------------------------------------------------------------------
newtype Token =
    Token {
      token :: String
    }

derive instance genericToken :: Generic Token _
derive instance newtypeToken :: Newtype Token _

--------------------------------------------------------------------------------
_Token :: Iso' Token { token :: String}
_Token = _Newtype

--------------------------------------------------------------------------------
instance decodeChannel :: Decode Channel where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodeChannel :: Encode Channel where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
derive instance eqChannel :: Eq Channel
derive instance ordChannel :: Ord Channel
instance showChannel :: Show Channel where show = genericShow
instance decodeMsg :: Decode Msg where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodeMsg :: Encode Msg where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
derive instance eqMsg :: Eq Msg
derive instance ordMsg :: Ord Msg
instance showMsg :: Show Msg where show = genericShow
instance decodeNewMsg :: Decode NewMsg where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodeNewMsg :: Encode NewMsg where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
derive instance eqNewMsg :: Eq NewMsg
derive instance ordNewMsg :: Ord NewMsg
instance showNewMsg :: Show NewMsg where show = genericShow
instance decodePriority :: Decode Priority where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodePriority :: Encode Priority where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
derive instance eqPriority :: Eq Priority
derive instance ordPriority :: Ord Priority
instance showPriority :: Show Priority where show = genericShow
instance decodeUserPass :: Decode UserPass where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodeUserPass :: Encode UserPass where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
derive instance eqUserPass :: Eq UserPass
derive instance ordUserPass :: Ord UserPass
instance showUserPass :: Show UserPass where show = genericShow
instance decodeToken :: Decode Token where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })
instance encodeToken :: Encode Token where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token
instance showToken :: Show Token where show = genericShow