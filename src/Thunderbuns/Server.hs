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
import Network.HTTP.Types (statusCode)
import Network.Wai (rawPathInfo, responseStatus)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Thunderbuns.Config
import Thunderbuns.Config.Server (HasServerConfig, port, staticRoot)
import Thunderbuns.Logging

type DebugAPI = Get '[ JSON] [T.Text] -- list debug levels

debugAPI :: Proxy DebugAPI
debugAPI = Proxy

debugServerT :: ServerT DebugAPI (ReaderT Env Handler)
debugServerT = do
  debugIO "debug server entered"
  pure ["asdf", "jkl"]

debugServer :: Env -> Server DebugAPI
debugServer e = hoistServer debugAPI (toHandler e) debugServerT

type StaticAPI = Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

staticServer :: (HasServerConfig e) => e -> Server StaticAPI
staticServer e = serveDirectoryWebApp (T.unpack $ view staticRoot e)

type CombinedAPI = "debug" :> DebugAPI :<|> "static" :> StaticAPI

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

combinedServer :: Env -> Server CombinedAPI
combinedServer e = debugServer e :<|> staticServer e

startApp :: ReaderT Env IO ()
startApp
  -- register ghc metrics - needs +RTS -T
 = do
  _ <- liftIO $ register ghcMetrics
  e <- ask
  let p = view (server . port) e
  infoIO ("Starting to serve on port " <> T.pack (show p))
  liftIO $ run (fromInteger p) (app e)
  where
    app e
      -- prometheus instrumentation: will instrument request latency by default
     =
      prometheus def $
      -- duration logging, request url context
      logware e $
      -- combined servant api
       \e' -> serve combinedAPI (combinedServer e')
    logware e appE req respond = do
      start <- SC.getSystemTime
      let ctx =
            M.singleton
              "req"
              (AT.Object
                 (M.singleton
                    "url"
                    (AT.String $ TE.decodeUtf8 $ rawPathInfo req)))
      lg <- logger "thunderbuns.server" ctx (view loggerL e)
      let e' = over loggerL (const lg) e
      appE e' req $ \res -> do
        responded <- respond res
        let st = responseStatus res
        (obj, msg) <- duration start
        let obj' =
              M.insert
                "res"
                (AT.Object $
                 M.singleton "status" $ AT.Number $ fromIntegral $ statusCode st)
                obj
        logM INFO obj' msg lg
        pure responded

toHandler :: e -> ReaderT e Handler a -> Handler a
toHandler e r = runReaderT r e
