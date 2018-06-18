{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server.Channel where

import Control.Monad.Reader
import Servant
import Thunderbuns.Channel
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.Server.Types
import Jose.Jwt (JwtClaims)

type ChannelAPI = Get '[ JSON] [Channel] :<|> ReqBody '[JSON] Channel :> PutNoContent '[JSON] NoContent

channelAPI :: Proxy ChannelAPI
channelAPI = Proxy

channelServerT :: HasDbConnection r => JwtClaims -> ServerT ChannelAPI (ReaderT r Handler)
channelServerT _ = mapError list :<|> addChannel'
  where
    addChannel' c = mapError $ do
      validateTB c >>= addChannel
      pure NoContent
