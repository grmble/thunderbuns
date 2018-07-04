{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server.Channel where

import Data.Maybe (fromJust)
import Data.UUID (fromText)
import qualified Data.Text as T
import Servant
import Thunderbuns.Auth.Types
import Thunderbuns.Channel
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.Logging (HasLogger)
import Thunderbuns.Server.Types
import Thunderbuns.Validate (uuidValidator)

type ChannelAPI
   = Get '[ JSON] [Channel]
   :<|> ReqBody '[ JSON] Channel :> PutNoContent '[ JSON] NoContent
   :<|> Capture "channel" T.Text :> Get '[ JSON] [Msg]
   :<|> Capture "channel" T.Text :> "before" :> Capture "pk" T.Text :> Get '[ JSON] [Msg]
   :<|> Capture "channel" T.Text :> ReqBody '[ JSON] NewMsg :> PutNoContent '[ JSON] NoContent

channelAPI :: Proxy ChannelAPI
channelAPI = Proxy

channelServer ::
     (HasDbConnection r, HasEventChannel r, HasLogger r) => r -> Claims -> Server ChannelAPI
channelServer r claims =
  mapError list r :<|> addChannel' :<|> messages' :<|> messagesBefore' :<|> addMessage'
  where
    addChannel' c =
      flip mapError r $ do
        validateTB c >>= addChannel
        pure NoContent
    addMessage' c (NewMsg s) =
      flip mapError r $ do
        mkMsg (Channel c) (jwtSub claims) s >>= validateTB >>= addMessage
        pure NoContent
    messages' c = mapError (validateTB (Channel c) >>= messages) r
    messagesBefore' c pk = flip mapError r $ do
      cv <- validateTB (Channel c)
      pkv <- validateTB' uuidValidator pk
      messagesBefore cv (fromJust . fromText <$> pkv)
