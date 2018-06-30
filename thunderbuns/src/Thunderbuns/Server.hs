{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server where

import Control.Lens (view)
import Control.Monad.Reader
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.System as SC
import Network.HTTP.Types (statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.Server.Experimental.Auth
import Thunderbuns.Auth.Types
import Thunderbuns.Config
import Thunderbuns.Config.Server (HasServerConfig, port, staticRoot)
import Thunderbuns.Logging
import Thunderbuns.Server.Auth
import Thunderbuns.Server.Channel
import Thunderbuns.Server.Debug
import Thunderbuns.Server.Event

type WebAPI
   = "auth" :> AuthAPI
   :<|> "channel" :> AuthProtect "jwt-auth" :> ChannelAPI
   :<|> "debug" :> AuthProtect "jwt-auth" :> DebugAPI

webAPI :: Proxy WebAPI
webAPI = Proxy

webServer :: Env -> Server WebAPI
webServer r = authServer r :<|> channelServer r :<|> debugServer r

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

staticServer :: (HasServerConfig r) => r -> Server StaticAPI
staticServer r = serveDirectoryWebApp (T.unpack $ view staticRoot r)

type CombinedAPI = WebAPI :<|> StaticAPI :<|> EventAPI

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

combinedServer :: Env -> Server CombinedAPI
combinedServer e = webServer e :<|> staticServer e :<|> eventServer e

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
      logResponseTime e serveCombinedAPI

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "jwt-auth") = Claims

type ApplicationE = Env -> Application

type MiddlewareE = Env -> ApplicationE -> Application

serveCombinedAPI :: ApplicationE
serveCombinedAPI r =
  serveWithContext combinedAPI (jwtContext r) (combinedServer r)
  where
    jwtContext e = jwtAuthHandler e :. EmptyContext

logResponseTime :: MiddlewareE
logResponseTime e appE req respond = do
  start <- SC.getSystemTime
  flip runReaderT e $
    localLogger "thunderbuns.server" (requestLoggingContext req) $
    ReaderT $ \e' ->
      appE e' req $ \res -> do
        responded <- respond res
        end <- SC.getSystemTime
        let (obj, msg) = duration start end
        let obj' = responseLoggingContext res obj
        runReaderT (logRecord INFO obj' msg) e'
        pure responded

requestLoggingContext :: Request -> AT.Object
requestLoggingContext req =
  M.singleton
    "req"
    (AT.Object
       (M.fromList
          [ ("remoteAddress", AT.String $ T.pack $ show $ remoteHost req)
          , ("method", AT.String $ TE.decodeUtf8 $ requestMethod req)
          , ("url", AT.String $ TE.decodeUtf8 $ rawPathInfo req)
          ]))

responseLoggingContext :: Response -> AT.Object -> AT.Object
responseLoggingContext res =
  M.insert
    "res"
    (AT.Object $
     M.singleton "status" $
     AT.Number $ fromIntegral $ statusCode $ responseStatus res)
