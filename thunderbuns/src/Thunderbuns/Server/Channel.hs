{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server.Channel where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Jose.Jwt (JwtClaims, jwtSub)
import Servant
import Thunderbuns.Channel
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.Server.Types

type ChannelAPI
   = Get '[ JSON] [Channel]
   :<|> ReqBody '[ JSON] Channel :> PutNoContent '[ JSON] NoContent
   :<|> Capture "channel" T.Text :> Get '[JSON] [Msg]
   :<|> Capture "channel" T.Text :> ReqBody '[ JSON] NewMsg :> PutNoContent '[ JSON] NoContent

channelAPI :: Proxy ChannelAPI
channelAPI = Proxy

channelServerT ::
     HasDbConnection r => JwtClaims -> ServerT ChannelAPI (ReaderT r Handler)
channelServerT claims = mapError list :<|> addChannel' :<|> messages' :<|> addMessage'
  where
    addChannel' c =
      mapError $ do
        validateTB c >>= addChannel
        pure NoContent
    addMessage' c (NewMsg s) =
      mapError $ do
        mkMsg (Channel c) (fromMaybe "" (jwtSub claims)) s >>= validateTB >>= addMessage
        pure NoContent
    messages' c = mapError $
      validateTB (Channel c) >>= messages
