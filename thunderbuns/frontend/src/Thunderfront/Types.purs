-- | Model/Msg types for the Thunderbuns frontend
module Thunderfront.Types where

import Prelude

import Bonsai.Forms (FormMsg, FormModel)
import Bonsai.Forms.Model (emptyFormModel)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Thunderbuns.WebAPI.Types (Channel(..), _Channel)
import Thunderbuns.WebAPI.Types as WT

-- | Main application model
-- |
-- | It has
-- |
-- | * an optional jwt token
-- | * a list of known channels, of which one is active
-- | * state for the active channel (list of messages, ...)
-- | * an input for new messages
-- | * a login form model (to acquite a token)
-- |
-- | There are lenses for all record properties, and HasX typeclasses
-- | for all the record types involved.
newtype Model =
  Model
  { jwtToken :: Maybe String
  , channelList :: ChannelList
  , channelModel :: ChannelModel
  , formModels :: FormModels
  , currentView :: CurrentView
  }

derive instance newtypeModel :: Newtype Model _
derive instance genericModel :: Generic Model _
-- instance showModel :: Show Model where
--   show = genericShow

emptyModel :: Model
emptyModel =
  Model { jwtToken: Nothing
        , channelList: ChannelList { activeChannel: (Channel { channelName: "Default" })
                                   , channels: [Channel { channelName: "Default" } ]}
        , channelModel: ChannelModel { messages: []}
        , formModels: FormModels { inputModel: ""
                                 , loginFormModel: emptyFormModel}
        , currentView: ChannelView }


jwtToken :: Lens' Model (Maybe String)
jwtToken = _Newtype <<< prop (SProxy :: SProxy "jwtToken")

channelList :: Lens' Model ChannelList
channelList = _Newtype <<< prop (SProxy :: SProxy "channelList")

channelModel :: Lens' Model ChannelModel
channelModel = _Newtype <<< prop (SProxy :: SProxy "channelModel")

formModels :: Lens' Model FormModels
formModels = _Newtype <<< prop (SProxy :: SProxy "formModels")

currentView :: Lens' Model CurrentView
currentView = _Newtype <<< prop (SProxy :: SProxy "currentView")

-- | Channel list
-- |
-- | Models the list (or tree) of known channels
-- | and the currently active one
newtype ChannelList =
  ChannelList
  { activeChannel :: Channel
  , channels :: Array Channel
  }

derive instance newtypeChannelList :: Newtype ChannelList _
derive instance genericChannelList :: Generic ChannelList _
instance showChannelList :: Show ChannelList where
  show x = show (map (view channelName) (view channels x))

channelName :: Lens' Channel String
channelName = _Channel <<< prop (SProxy :: SProxy "channelName")

activeChannel :: Lens' ChannelList Channel
activeChannel = _Newtype <<< prop (SProxy :: SProxy "activeChannel")

channels :: Lens' ChannelList (Array Channel)
channels = _Newtype <<< prop (SProxy :: SProxy "channels")

-- | Channel Model
-- |
-- | Models the active channel where the messages are shown
newtype ChannelModel =
  ChannelModel
  { messages :: Array WT.Msg
  }

derive instance newtypeChannelModel :: Newtype ChannelModel _
derive instance genericChannelModel :: Generic ChannelModel _
-- instance showChannelModel :: Show ChannelModel where
--  show = map (show <<< viewgenericShow

messages :: Lens' ChannelModel (Array WT.Msg)
messages = _Newtype <<< prop (SProxy :: SProxy "messages")

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


-- | Messages
data Msg
  = JwtTokenMsg (Maybe String)
  | MessageInputMsg String
  | LoginFormMsg FormMsg
  | ChannelListMsg (Array Channel)
  | ActiveChannelMsg Channel
  | MessageMsg (Array WT.Msg)
  | NewMessageMsg String
  | CurrentViewMsg CurrentView

derive instance genericMsg :: Generic Msg _
-- instance showMsg :: Show Msg where
--  show = genericShow
