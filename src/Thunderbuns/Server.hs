{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server where

import Control.Lens (view)
import Control.Monad.Reader
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Network.Wai.Handler.Warp
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

staticServer :: (HasServerConfig e, HasLogger e) => e -> Server StaticAPI
staticServer e = serveDirectoryWebApp (T.unpack $ view staticRoot e)

type CombinedAPI = "debug" :> DebugAPI :<|> "static" :> StaticAPI

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

combinedServer :: Env -> Server CombinedAPI
combinedServer e = debugServer e :<|> staticServer e

startApp :: ReaderT Env IO ()
startApp = do
  e <- ask
  let p = view (server . port) e
  infoIO ("Starting to serve on port " <> T.pack (show p))
  liftIO $ run (fromInteger p) (app e)
  where
    app e = serve combinedAPI (combinedServer e)

toHandler :: e -> ReaderT e Handler a -> Handler a
toHandler e r = runReaderT r e
