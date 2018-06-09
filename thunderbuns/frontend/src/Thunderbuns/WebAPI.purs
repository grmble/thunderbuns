-- File auto generated by servant-purescript! --
module Thunderbuns.WebAPI where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (ask, class MonadAsk)
import Data.Argonaut.Core (stringify)
import Data.Array (catMaybes, null)
import Data.Maybe (Maybe, Maybe(..))
import Data.Nullable (toNullable)
import Data.String (joinWith)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit)
import Prim (Array, String)
import Servant.PureScript.Affjax (AjaxError, affjax, defaultRequest)
import Servant.PureScript.Settings (SPSettingsDecodeJson_(..), SPSettingsEncodeJson_(..), SPSettings_(..), gDefaultToURLPiece)
import Servant.PureScript.Util (encodeHeader, encodeListQuery, encodeQueryItem, encodeURLPiece, getResult)
import Thunderbuns.WebAPI.Types (Priority, Token, UserPass)

newtype SPParams_ = SPParams_ { baseURL :: String
                              }

postAuth :: forall eff m.
            MonadAsk (SPSettings_ SPParams_) m => MonadError AjaxError m => MonadAff ( ajax :: AJAX | eff) m
            => UserPass -> m Token
postAuth reqBody = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let queryString = ""
  let reqUrl = baseURL <> "auth" <> queryString
  let reqHeaders =
        []
  let encodeJson = case spOpts_.encodeJson of SPSettingsEncodeJson_ e -> e
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 , content = toNullable <<< Just <<< stringify <<< encodeJson $ reqBody
                 }
  affResp <- affjax affReq
  let decodeJson = case spOpts_.decodeJson of SPSettingsDecodeJson_ d -> d
  getResult affReq decodeJson affResp

getDebug :: forall eff m.
            MonadAsk (SPSettings_ SPParams_) m => MonadError AjaxError m => MonadAff ( ajax :: AJAX | eff) m
            => String -> m (Array (Tuple String (Maybe Priority)))
getDebug authorization = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "GET"
  let queryString = ""
  let reqUrl = baseURL <> "debug" <> queryString
  let reqHeaders =
        [{ field : "Authorization" , value : encodeHeader spOpts_' authorization
         }]
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 }
  affResp <- affjax affReq
  let decodeJson = case spOpts_.decodeJson of SPSettingsDecodeJson_ d -> d
  getResult affReq decodeJson affResp

postDebug :: forall eff m.
             MonadAsk (SPSettings_ SPParams_) m => MonadError AjaxError m => MonadAff ( ajax :: AJAX | eff) m
             => String -> Array (Tuple String (Maybe Priority)) -> m Unit
postDebug authorization reqBody = do
  spOpts_' <- ask
  let spOpts_ = case spOpts_' of SPSettings_ o -> o
  let spParams_ = case spOpts_.params of SPParams_ ps_ -> ps_
  let baseURL = spParams_.baseURL
  let httpMethod = "POST"
  let queryString = ""
  let reqUrl = baseURL <> "debug" <> queryString
  let reqHeaders =
        [{ field : "Authorization" , value : encodeHeader spOpts_' authorization
         }]
  let encodeJson = case spOpts_.encodeJson of SPSettingsEncodeJson_ e -> e
  let affReq = defaultRequest
                 { method = httpMethod
                 , url = reqUrl
                 , headers = defaultRequest.headers <> reqHeaders
                 , content = toNullable <<< Just <<< stringify <<< encodeJson $ reqBody
                 }
  _ <- affjax affReq
  pure unit
