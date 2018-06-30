{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Streaming events to the client and search servers
module Thunderbuns.Server.Event where

import Control.Lens (view)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode)
import Data.Binary.Builder (fromLazyByteString)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Servant
import Servant.Server.Experimental.Auth ()
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.Logging (logDebug)
import UnliftIO.STM (TChan, atomically, dupTChan, readTChan)

type EventAPI = "events" :> AuthProtect "jwt-auth" :> Raw

eventAPI :: Proxy EventAPI
eventAPI = Proxy

-- | SSE Endpoint
eventServer :: Env -> Server EventAPI
eventServer r _ = Tagged $ eventApp r

eventApp :: Env -> Application
eventApp r req respond = do
  chan <- listeningChannel r
  eventSourceAppIO (nextEvent chan) req $ \res -> respond res
  where
    nextEvent chan = do
      runReaderT (logDebug "waiting for the listening channel") r
      atomically (toServerEvent <$> readTChan chan)

listeningChannel :: Env -> IO (TChan Msg)
listeningChannel r = do
  runReaderT (logDebug "creating the listening channel") r
  atomically (dupTChan (view eventBroadcast r))

toServerEvent :: Msg -> ServerEvent
toServerEvent msg =
  ServerEvent
    { eventName = Nothing
    , eventId = Nothing
    , eventData = [fromLazyByteString $ encode msg]
    }
