{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server where

import Control.Lens (over, view)
import Control.Monad.Reader
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.System as SC
import Jose.Jwt
import Network.HTTP.Types (statusCode)
import Network.Wai (rawPathInfo, remoteHost, requestMethod, responseStatus)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.Server.Experimental.Auth
import Thunderbuns.Config
import Thunderbuns.Config.Server (HasServerConfig, port, staticRoot)
import Thunderbuns.Logging
import Thunderbuns.Server.Auth
import Thunderbuns.Server.Channel
import Thunderbuns.Server.Debug

authServer :: Env -> Server AuthAPI
authServer r = hoistServer authAPI (toHandler r) userServerT

debugServer :: Env -> JwtClaims -> Server DebugAPI
debugServer r cs = hoistServer debugAPI (toHandler r) (debugServerT cs)

channelServer :: Env -> JwtClaims -> Server ChannelAPI
channelServer r cs = hoistServer channelAPI (toHandler r) (channelServerT cs)

type WebAPI
   = "auth" :> AuthAPI :<|> "channel" :> AuthProtect "jwt-auth" :> ChannelAPI :<|> "debug" :> AuthProtect "jwt-auth" :> DebugAPI

webAPI :: Proxy WebAPI
webAPI = Proxy

webServer :: Env -> Server WebAPI
webServer r = authServer r :<|> channelServer r :<|> debugServer r

type StaticAPI = Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

staticServer :: (HasServerConfig e) => e -> Server StaticAPI
staticServer e = serveDirectoryWebApp (T.unpack $ view staticRoot e)

type CombinedAPI = WebAPI :<|> "static" :> StaticAPI

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

combinedServer :: Env -> Server CombinedAPI
combinedServer e = webServer e :<|> staticServer e

startApp :: (MonadTLogger m, MonadIO m) => ReaderT Env m ()
startApp
  -- register ghc metrics - needs +RTS -T
 = do
  _ <- liftIO $ register ghcMetrics
  e <- ask
  let p = view (server . port) e
  lift $ logInfo ("Starting to serve on port " <> T.pack (show p))
  liftIO $ run (fromInteger p) (app e)
  where
    app e
      -- prometheus instrumentation: will instrument request latency by default
     =
      prometheus def $
      -- duration logging, request url context
      logware e $
      -- combined servant api
       \e' -> serveWithContext combinedAPI (jwtContext e') (combinedServer e')
    jwtContext e = jwtAuthHandler e :. EmptyContext
    logware e appE req respond = do
      start <- SC.getSystemTime
      let ctx =
            M.singleton
              "req"
              (AT.Object
                 (M.fromList
                    [ ( "remoteAddress"
                      , AT.String $ T.pack $ show $ remoteHost req)
                    , ("method", AT.String $ TE.decodeUtf8 $ requestMethod req)
                    , ("url", AT.String $ TE.decodeUtf8 $ rawPathInfo req)
                    ]))
      lg <- runReaderT (childLogger "thunderbuns.server" ctx) (view loggerL e)
      let e' = over loggerL (const lg) e
      appE e' req $ \res -> do
        responded <- respond res
        let st = responseStatus res
        end <- SC.getSystemTime
        let (obj, msg) = duration start end
        let obj' =
              M.insert
                "res"
                (AT.Object $
                 M.singleton "status" $ AT.Number $ fromIntegral $ statusCode st)
                obj
        runReaderT (logRecord INFO obj' msg) lg
        pure responded

toHandler :: r -> ReaderT r Handler a -> Handler a
toHandler r m = runReaderT m r

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "jwt-auth") = JwtClaims
