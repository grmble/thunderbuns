{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Streaming events to the client and search servers
module Thunderbuns.Server.Event where

import Control.Lens (view)
import Network.Wai.EventSource (eventSourceAppIO)
import Servant
import Servant.Server.Experimental.Auth ()
import Thunderbuns.Config
import UnliftIO.STM (atomically, dupTChan, readTChan)

type EventAPI = "events" :> AuthProtect "jwt-auth" :> Raw

eventAPI :: Proxy EventAPI
eventAPI = Proxy

eventServer :: Env -> Server EventAPI
eventServer r _ =
  Tagged $
  eventSourceAppIO $
  atomically $ do
    let bchan = view eventBroadcast r
    chan <- dupTChan bchan
    readTChan chan
