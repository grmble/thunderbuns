{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Streaming events to the client and search servers
module Thunderbuns.Server.Event where

import Control.Lens (review, view)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (encode)
import Data.Binary.Builder (fromByteString, fromLazyByteString)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Servant
import Servant.Server.Experimental.Auth ()
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.Event
import Thunderbuns.Logging (logDebug)
import Thunderbuns.OrderedUUID (_OrderedUUID)
import Thunderbuns.Server.Types (mapErrorIO, validateTB')
import Thunderbuns.Validate (uuidValidator)
import UnliftIO.STM (TChan, atomically, dupTChan, readTChan, writeTChan)

-- can not use OrderedUUID here
-- when an empty string is given, the parse fails, and that kills the script
-- instead of giving a Nothing
type EventAPI
   = "events" :> AuthProtect "jwt-auth" :> QueryParam "lastEventId" T.Text :> Raw

eventAPI :: Proxy EventAPI
eventAPI = Proxy

-- | SSE Endpoint
eventServer :: Env -> Server EventAPI
eventServer r _ lastId = Tagged $ eventApp r lastId

eventApp :: Env -> Maybe T.Text -> Application
eventApp r lastId req respond = do
  chan <- listeningChannel r
  for_ (lastId >>= uuid) $ \created -> do
    runReaderT (logDebug "sending events ...") r
    rows <- mapErrorIO (validateTB' uuidValidator created >>= eventsSince) r
    for_ rows (atomically . writeTChan chan)
  eventSourceAppIO (nextEvent chan) req $ \res -> respond res
  where
    uuid s =
      if s == ""
      then Nothing
      else Just $ review _OrderedUUID (T.encodeUtf8 s)
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
    , eventId = Just (fromByteString $ view _OrderedUUID (created msg))
    , eventData = [fromLazyByteString $ encode msg]
    }
