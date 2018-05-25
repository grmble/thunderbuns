-- | Model/Msg types for the Thunderbuns frontend
module Thunderfront.Types where

import Prelude

import Bonsai.Forms (FormMsg, FormModel)
import Bonsai.Forms.Model (emptyFormModel)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

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
  }

emptyModel :: Model
emptyModel =
  Model { jwtToken: Nothing
        , channelList: ChannelList { activeChannel: "default"
                                   , channels: ["default"]}
        , channelModel: ChannelModel { messages: []}
        , formModels: FormModels { inputModel: emptyFormModel
                                 , loginFormModel: emptyFormModel}}

derive instance newtypeModel :: Newtype Model _
derive instance genericModel :: Generic Model _
instance showModel :: Show Model where
  show = genericShow


jwtToken' :: Lens' Model (Maybe String)
jwtToken' = _Newtype <<< prop (SProxy :: SProxy "jwtToken")

channelList' :: Lens' Model ChannelList
channelList' = _Newtype <<< prop (SProxy :: SProxy "channelList")

channelModel' :: Lens' Model ChannelModel
channelModel' = _Newtype <<< prop (SProxy :: SProxy "channelModel")

formModels :: Lens' Model FormModels
formModels = _Newtype <<< prop (SProxy :: SProxy "formModels")

class HasJwtToken a where
  jwtToken :: Lens' a (Maybe String)
instance jwtTokenModel :: HasJwtToken Model where
  jwtToken = jwtToken'


-- | Channel list
-- |
-- | Models the list (or tree) of known channels
-- | and the currently active one
newtype ChannelList =
  ChannelList
  { activeChannel :: String
  , channels :: Array String
  }

derive instance newtypeChannelList :: Newtype ChannelList _
derive instance genericChannelList :: Generic ChannelList _
instance showChannelList :: Show ChannelList where
  show = genericShow

activeChannel :: Lens' ChannelList String
activeChannel = _Newtype <<< prop (SProxy :: SProxy "activeChannel")

channels :: Lens' ChannelList (Array String)
channels = _Newtype <<< prop (SProxy :: SProxy "channels")

class HasChannelList a where
  channelList :: Lens' a ChannelList
instance channelListHasChannelList :: HasChannelList ChannelList where
  channelList = id
instance modelHasChannelList :: HasChannelList Model where
  channelList = channelList'

-- | Channel Model
-- |
-- | Models the active channel where the messages are shown
newtype ChannelModel =
  ChannelModel
  { messages :: Array String
  }

derive instance newtypeChannelModel :: Newtype ChannelModel _
derive instance genericChannelModel :: Generic ChannelModel _
instance showChannelModel :: Show ChannelModel where
  show = genericShow

messages :: Lens' ChannelModel (Array String)
messages = _Newtype <<< prop (SProxy :: SProxy "messages")

class HasChannelModel a where
  channelModel :: Lens' a ChannelModel
instance channelModelHasChannelModel :: HasChannelModel ChannelModel where
  channelModel = id
instance modelHasChannelModel :: HasChannelModel Model where
  channelModel = channelModel'

-- | Form models for the unique forms
-- |
-- | * input model: form model for the new message input
-- | * loginFormModel: model for the login form
newtype FormModels =
  FormModels
  { inputModel :: FormModel
  , loginFormModel :: FormModel
  }

derive instance newtypeFormModels :: Newtype FormModels _
derive instance genericFormModels :: Generic FormModels _
instance showFormModels :: Show FormModels where
  show = genericShow

inputModel' :: Lens' FormModels FormModel
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
  inputModel :: Lens' a FormModel
instance formModelsHasInputModel :: HasInputModel FormModels where
  inputModel = inputModel'
instance modelHasInputModel :: HasInputModel Model where
  inputModel = formModels <<< inputModel'



-- | Messages
data Msg
  = JwtTokenMsg (Maybe String)
  | InputFormMsg FormMsg
  | LoginFormMsg FormMsg

derive instance genericMsg :: Generic Msg _
-- XXX FormModel show instance
-- instance showMsg :: Show Msg where
--   show = genericShow
