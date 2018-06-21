-- File auto generated by purescript-bridge! --
module Thunderbuns.WebAPI.Types where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Prim (String)

import Prelude
import Data.Generic (class Generic)

newtype Channel =
    Channel {
      channelName :: String
    }

derive instance genericChannel :: Generic Channel

derive instance newtypeChannel :: Newtype Channel _


--------------------------------------------------------------------------------
_Channel :: Iso' Channel { channelName :: String}
_Channel = _Newtype

--------------------------------------------------------------------------------
newtype Msg =
    Msg {
      channel :: Channel
    , pk :: String
    , user :: String
    , msg :: String
    }

derive instance genericMsg :: Generic Msg

derive instance newtypeMsg :: Newtype Msg _


--------------------------------------------------------------------------------
_Msg :: Iso' Msg { channel :: Channel, pk :: String, user :: String, msg :: String}
_Msg = _Newtype

--------------------------------------------------------------------------------
newtype NewMsg =
    NewMsg {
      msg :: String
    }

derive instance genericNewMsg :: Generic NewMsg

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

derive instance genericPriority :: Generic Priority


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

derive instance genericUserPass :: Generic UserPass

derive instance newtypeUserPass :: Newtype UserPass _


--------------------------------------------------------------------------------
_UserPass :: Iso' UserPass { user :: String, pass :: String}
_UserPass = _Newtype

--------------------------------------------------------------------------------
newtype Token =
    Token {
      token :: String
    }

derive instance genericToken :: Generic Token

derive instance newtypeToken :: Newtype Token _


--------------------------------------------------------------------------------
_Token :: Iso' Token { token :: String}
_Token = _Newtype

--------------------------------------------------------------------------------
