-- | Model types for the Thunderbuns frontend
module Thunderfront.Types.Model where

import Prelude

import Bonsai.Forms (FormModel)
import Bonsai.Forms.Model (emptyFormModel)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Set as S
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Thunderfront.WebSocket (WebSocket)
import Thunderfront.Types.WS as WS

-- | Main application model
-- |
-- | It has
-- |
-- | * an optional web socket (only set if currently connected)
-- | * a list of known channels
-- | * a map of messages by channel
-- | * a list of generic messages (i.e. messages without channel)
-- | * an input for new messages
-- |
-- | There are lenses for all record properties, and HasX typeclasses
-- | for all the record types involved.
-- |
-- | There are also various form models to hold form state.
-- |
-- | As leftover from old code, there are unused types for login form
-- | and view switching.
newtype Model =
  Model
  { webSocket :: Maybe WebSocket

  , maxRequestID :: WS.RequestID
  , activeRequests :: S.Set WS.RequestID

  , activeChannel :: Maybe WS.Channel
  , channelMessages :: M.Map WS.Channel (M.Map String NickAndMsg)
  , messages :: M.Map String (Tuple String String)

  , formModels :: FormModels
  , currentView :: CurrentView
  }

emptyModel :: Model
emptyModel =
  Model { webSocket: Nothing
        , maxRequestID: WS.RequestID 0
        , activeRequests: S.empty
        , activeChannel: Nothing
        , channelMessages: M.empty
        , messages: M.empty
        , formModels: FormModels { inputModel: ""
                                 , loginFormModel: emptyFormModel}
        , currentView: ChannelView }

derive instance newtypeModel :: Newtype Model _
derive instance genericModel :: Generic Model _
instance showModel :: Show Model where show = genericShow

-- | Helper for record with nick and msg
newtype NickAndMsg =
  NickAndMsg { uuid :: String
             , nick :: WS.Nick
             , msg :: String
             , timestamp :: String
             }
derive instance newtypeNickAndMsg :: Newtype NickAndMsg _
derive instance genericNickAndMsg :: Generic NickAndMsg _
instance showNickAndMsg :: Show NickAndMsg where show = genericShow


webSocket :: Lens' Model (Maybe WebSocket)
webSocket = _Newtype <<< prop (SProxy :: SProxy "webSocket")

formModels :: Lens' Model FormModels
formModels = _Newtype <<< prop (SProxy :: SProxy "formModels")

currentView :: Lens' Model CurrentView
currentView = _Newtype <<< prop (SProxy :: SProxy "currentView")

channelName :: Lens' WS.Channel String
channelName = WS._Channel

activeChannel :: Lens' Model (Maybe WS.Channel)
activeChannel = _Newtype <<< prop (SProxy :: SProxy "activeChannel")

channelMessages :: Lens' Model (M.Map WS.Channel (M.Map String NickAndMsg))
channelMessages = _Newtype <<< prop (SProxy :: SProxy "channelMessages")

messages :: Lens' Model (M.Map String (Tuple String String))
messages = _Newtype <<< prop (SProxy :: SProxy "messages")

activeRequests :: Lens' Model (S.Set WS.RequestID)
activeRequests = _Newtype <<< prop (SProxy :: SProxy "activeRequests")

maxRequestID :: Lens' Model WS.RequestID
maxRequestID = _Newtype <<< prop (SProxy :: SProxy "maxRequestID")

maxRequestID' :: Lens' Model Int
maxRequestID' = maxRequestID <<< WS._RequestID

-- | Form models for the unique forms
-- |
-- | * input model: form model for the new message input
-- | * loginFormModel: model for the login form
newtype FormModels =
  FormModels
  { inputModel :: String
  , loginFormModel :: FormModel
  }

derive instance newtypeFormModels :: Newtype FormModels _
derive instance genericFormModels :: Generic FormModels _
instance showFormModels :: Show FormModels where
  show = genericShow

inputModel' :: Lens' FormModels String
inputModel' = _Newtype <<< prop (SProxy :: SProxy "inputModel")

loginFormModel' :: Lens' FormModels FormModel
loginFormModel' = _Newtype <<< prop (SProxy :: SProxy "loginFormModel")

class HasLoginFormModel a where
  loginFormModel :: Lens' a FormModel
instance formModelLoginFormModel :: HasLoginFormModel FormModels where
  loginFormModel = loginFormModel'
instance modelLoginFormModel :: HasLoginFormModel Model where
  loginFormModel = formModels <<< loginFormModel'

class HasInputModel a where
  inputModel :: Lens' a String
instance formModelsHasInputModel :: HasInputModel FormModels where
  inputModel = inputModel'
instance modelHasInputModel :: HasInputModel Model where
  inputModel = formModels <<< inputModel'


-- ! currently active menu
data CurrentView
  = ChannelView
  | DebugView

derive instance genericCurrentView :: Generic CurrentView _
derive instance eqCurrentView :: Eq CurrentView
instance showCurrentView :: Show CurrentView where
  show = genericShow
