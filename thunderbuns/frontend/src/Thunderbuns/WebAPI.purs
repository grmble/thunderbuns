-- | Thunderbuns.WebAPI
--
-- This used to be autogenerated by servant-purescript,
-- but only purescript-bridge works with purescript 0.12
-- as of now, so this is hand coded
--
-- Directly parsing to Argonaut JSON does not work - it insists
-- on calling JSON.parse on the input, even for error returns.
-- So we always read a string and decode on our own.
module Thunderbuns.WebAPI where

import Prelude

import Bonsai.DOM (affF, effF)
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', view)
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (catchError, error, message, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign, readArray, readString)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericDecodeJSON, genericEncode)
import Foreign.Generic.Class (class GenericEncode, class GenericDecode)
import Foreign.Generic.Types (Options)
import Foreign.Index ((!))
import Global.Unsafe (unsafeEncodeURIComponent)
import Network.HTTP.Affjax (Affjax, AffjaxRequest, AffjaxResponse, URL, affjax, defaultRequest, post)
import Network.HTTP.Affjax.Request as ARq
import Network.HTTP.Affjax.Response as ARs
import Network.HTTP.RequestHeader as RH
import Network.HTTP.StatusCode (StatusCode(..))
import Thunderbuns.WebAPI.OrderedUUID (OrderedUUID(..))
import Thunderbuns.WebAPI.Types (Channel(..), Msg, NewMsg, Priority, Token, UserPass)
import Thunderfront.Types (Model)
import Toastr as T
import Unsafe.Coerce (unsafeCoerce)

newtype ApiParams
  = ApiParams
    { authorization :: String
    , baseURL :: String
    }

derive instance newtypeApiParams :: Newtype ApiParams _
derive instance genericApiParams :: Generic ApiParams _
instance showApiParams :: Show ApiParams where show = genericShow

instance hasApiParamsModel :: HasApiParams Model where
  apiParams = _Newtype <<< prop (SProxy :: SProxy "jwtToken") <<< jwtTokenApiParams "/"

jwtTokenApiParams :: String -> Iso' (Maybe String) ApiParams
jwtTokenApiParams rootURL =
  iso (maybe (mkap "") mkap) (sToM  <<< _.authorization <<< unwrap)
  where
    mkap a = ApiParams { authorization: a, baseURL: rootURL }
    sToM s = if s == "" then Nothing else Just s

authorization :: Lens' ApiParams String
authorization = _Newtype <<< prop (SProxy :: SProxy "authorization")

baseURL :: Lens' ApiParams URL
baseURL = _Newtype <<< prop (SProxy :: SProxy "baseURL")

gOpts :: Options
gOpts = defaultOptions { unwrapSingleConstructors = true }

gEncode :: forall a rep. Generic a rep => GenericEncode rep => a -> Json
gEncode a = unsafeCoerce $ genericEncode gOpts a

gDecode :: forall a rep m. Generic a rep => GenericDecode rep => MonadAff m => Json -> m a
gDecode json = liftAff $ affF $ genericDecode gOpts (unsafeCoerce json)

gDecodeArray :: forall a rep m. Generic a rep => GenericDecode rep => MonadAff m => Json -> m (Array a)
gDecodeArray json = liftAff $ affF $ do
  arr <- readArray (unsafeCoerce json)
  traverse (genericDecode gOpts) arr

gDecodeEvent :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> Effect a
gDecodeEvent ev = do
  effF $ (ev ! "data") >>= readString >>= genericDecodeJSON gOpts

urlPath :: String -> URL
urlPath = unsafeEncodeURIComponent

class HasApiParams a where
  apiParams :: Lens' a ApiParams
instance hasApiParamsApiParams :: HasApiParams ApiParams where
  apiParams = identity

requestURL :: forall r m. HasApiParams r => MonadReader r m => URL -> m URL
requestURL url =
  asks (view $ apiParams <<< baseURL) >>=
  (\b -> pure $ b <> url)

authHeader :: forall r m. HasApiParams r => MonadReader r m => m String
authHeader =
  asks (view $ apiParams <<< authorization) >>=
  (\a -> pure $ "Bearer " <> a)


postAuth :: forall r m. HasApiParams r => MonadReader r m => MonadAff m => UserPass -> m Token
postAuth body = do
  url <- requestURL "auth"
  rsp <- liftAff $ post ARs.string url (ARq.json (gEncode body))
  liftAff $ affF $ decodeJSON rsp.response


handleError
  :: forall r m a
  .  HasApiParams r => MonadReader r m => MonadAff m
  => AffjaxResponse String -> m a -> m a
handleError rsp ok = do
  let StatusCode code = rsp.status
  if (code / 100) == 2
     then ok
     else do
       liftEffect $ T.error rsp.response "Error"
       liftAff $ throwError $ error (show rsp.status <> ": " <> rsp.response)

decodeResponse
  :: forall r m a
  .  HasApiParams r => MonadReader r m => MonadAff m
  => Decode a
  => AffjaxResponse String -> m a
decodeResponse rsp = do
  handleError rsp $
    liftAff $ affF $ decodeJSON rsp.response

authGet
  :: forall r m
  .  HasApiParams r => MonadReader r m => MonadAff m
  => URL -> m (AffjaxResponse String)
authGet url = do
  url' <- requestURL url
  auth <- authHeader
  liftAff $ affjax' ARs.string $
    defaultRequest { method = Left GET
                   , url = url'
                   , headers = [ RH.RequestHeader "authorization" auth ]
                   , content = Nothing }

authPut
  :: forall r m a rep
  .  HasApiParams r => MonadReader r m => MonadAff m => Generic a rep => GenericEncode rep
  => URL -> a -> m Unit
authPut url body = do
  url' <- requestURL url
  liftEffect $ log ("authPut URL: " <> url')
  auth <- authHeader
  rsp <- liftAff $ affjax' ARs.string $
         defaultRequest { method = Left PUT
                        , url = url'
                        , headers = [ RH.RequestHeader "authorization" auth ]
                        , content = Just (ARq.json $ gEncode body) }
  handleError rsp (pure unit)

affjax' :: forall a. ARs.Response a -> AffjaxRequest -> Affjax a
affjax' a b =
  catchError (affjax a b) $ \e -> do
    liftEffect $ T.error (message e) "Error"
    throwError e


getChannel :: forall r m. HasApiParams r => MonadReader r m => MonadAff m => m (Array Channel)
getChannel =
  authGet "channel" >>= decodeResponse


putChannel :: forall r m. HasApiParams r => MonadReader r m => MonadAff m => Channel -> m Unit
putChannel = do
  authPut "channel"


getChannelByChannel
  :: forall r m
  .  HasApiParams r => MonadReader r m => MonadAff m
  => Channel -> m (Array Msg)
getChannelByChannel (Channel channel) =
  authGet ("channel/" <> urlPath channel.channelName) >>= decodeResponse

getChannelBefore
  :: forall r m
  .  HasApiParams r => MonadReader r m => MonadAff m
  => Channel -> OrderedUUID -> m (Array Msg)
getChannelBefore (Channel channel) (OrderedUUID created) =
  authGet ("channel/" <> urlPath channel.channelName <> "/before/" <> urlPath created) >>=
  decodeResponse

putChannelByChannel
  :: forall r m
  .  HasApiParams r => MonadReader r m => MonadAff m
  => NewMsg -> Channel -> m Unit
putChannelByChannel body (Channel channel) = do
  authPut ("channel/" <> urlPath channel.channelName) body

getDebug :: forall r m. HasApiParams r => MonadReader r m => MonadAff m => m (Array (Tuple String (Maybe Priority)))
getDebug = pure []

postDebug :: forall r m. HasApiParams r => MonadReader r m => MonadAff m => Array (Tuple String (Maybe Priority)) -> m Unit
postDebug reqBody = pure unit
