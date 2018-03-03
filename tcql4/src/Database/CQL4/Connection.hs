{-# LANGUAGE StrictData #-}

{- | CQL4 Connection

Basically, this is a socket to a Cassandra/ScyllaDB server.

Commands are sent over the socket (with a generated ID), response
messages are received over the socket.  The protocol is fully
asynchronous - messages can come in in any order.  They do contain
the ID of their command.

So the connection abstraction also allows to wait for the response
to a command.
-}
module Database.CQL4.Connection
  ( ConnectionIO
  , ConnectionData(..)
  , connectionData
  , hexdumpLogger
  , initConnection
  , closeConnection
  , runConnection
  , runConnection'
  , command
  ) where

import Control.Monad.Reader (ReaderT, ask, asks, liftIO, runReaderT)
import qualified Data.ByteString as B
import Data.ByteString.Hexdump (hexdump)
import qualified Data.ByteString.Lazy as LB
import Data.Conduit ((.|), runConduit)
import Data.Conduit.Cereal (conduitGet2, sourcePut)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Network (AppData, appSink, appSource)
import qualified Data.HashMap.Strict as M
import qualified Data.Serialize.Put as P
import Data.String (fromString)
import Database.CQL4.Internal.Protocol
import Database.CQL4.Types
import UnliftIO.Concurrent (ThreadId, forkIO, killThread)
import UnliftIO.Exception (bracket_)
import UnliftIO.STM

-- | A Connection is a ReaderT IO with mutable state
type ConnectionIO = ReaderT ConnectionData IO

-- | The data inside a ConnectionIO ReaderT
data ConnectionData = ConnectionData
  { connSocket :: AppData -- ^ conduit app data for the underlying socket
  , connStreamID :: TVar StreamID -- ^ counter for generating stream ids
  , connPending :: TVar (M.HashMap StreamID (TMVar Message)) -- ^ map of pending requests
  , connDispatcher :: TMVar ThreadId -- ^ dedicated thread to dispatch server messages
  , connLogger :: Logger -- ^ logging function for requests/responses
  }

-- | A logging function
--
-- Will log the bytestrings on the wire, for debugging
--
-- See hexdumpLogger or (const pure)
type Logger = String -> B.ByteString -> IO B.ByteString

-- | ConnectionData constructor for use with runReaderT
connectionData :: AppData -> IO ConnectionData
connectionData sock = do
  idVar <- newTVarIO 0
  pVar <- newTVarIO M.empty
  dVar <- newEmptyTMVarIO
  pure (ConnectionData sock idVar pVar dVar (const pure))

-- | Log a hexdump of the request/response
hexdumpLogger :: Logger
hexdumpLogger msg bs = do
  LB.putStr
    ((fromString msg) `LB.append` "\n" `LB.append` (hexdump (LB.fromStrict bs)))
  pure bs

-- | Run a ConnectionIO
--
-- A helper that calls runReaderT, initConnection and closeConnection
-- with correct bracketing.
runConnection :: AppData -> ConnectionIO a -> IO a
runConnection = runConnection' (const pure)

-- | Run a ConnectionIO - explicit version
runConnection' :: Logger -> AppData -> ConnectionIO a -> IO a
runConnection' logger sock conn = do
  cdata <- (\x -> x {connLogger = logger}) <$> connectionData sock
  runReaderT go cdata
  where
    go = do
      bracket_ initConnection closeConnection conn

-- | Initialize the connection.
--
-- Handles the CQL4 initialization (e.g. startup message, auth, ...)
-- and will block until this is completed.
--
-- After this initialization is complete, it is safe to use the
-- connection from multiple threads.
--
-- This will throw an error if there is a protocol problem
-- (e.g. if the server wants authentication)
initConnection :: ConnectionIO ()
initConnection = do
  cdata <- ask
  tid <- liftIO $ forkIO $ runReaderT dispatchResponses cdata
  liftIO $ atomically $ putTMVar (connDispatcher cdata) tid
  msg <- command startup
  case msg of
    ReadyMsg -> pure ()
    x -> liftIO $ messageError "unexpected response type" x

-- | Raise an IOError with a message string for a response
messageError :: String -> Message -> IO a
messageError str msg = ioError $ userError (str ++ ": " ++ show msg)

-- | Close the connection.
--
-- This will shutdown all pending request, but WILL NOT CLOSE
-- the underlying socket.  It is assumed that the socket
-- was bracketed in some way and will be closed by the calling code.
closeConnection :: ConnectionIO ()
closeConnection = do
  dispVar <- asks connDispatcher
  tid <- liftIO $ atomically $ tryReadTMVar dispVar
  case tid of
    Nothing -> pure ()
    Just x -> liftIO $ killThread x

-- | Generate the next stream id and a TMVar for the result
streamID :: ConnectionIO (StreamID, TMVar Message)
streamID = do
  idVar <- asks connStreamID
  pendingVar <- asks connPending
  p <- liftIO $ newEmptyTMVarIO
  let go pending sid = do
        if M.member sid pending
          then go pending (max 0 (1 + sid))
          else do
            writeTVar idVar (max 0 (1 + sid))
            writeTVar pendingVar (M.insert sid p pending)
            pure (sid, p)
  liftIO $
    atomically $ do
      sid <- readTVar idVar
      pending <- readTVar pendingVar
      go pending sid

-- | Send a command frame over the socket
command :: (StreamID -> P.Put) -> ConnectionIO Message
command cmd = do
  sock <- asks connSocket
  logger <- asks connLogger
  (sid, rslt) <- streamID
  liftIO $
    runConduit
      (sourcePut (cmd sid) .| CC.mapM (logger "request") .| appSink sock)
  liftIO $ atomically (takeTMVar rslt)

-- ! Dispatch server respones server responses - will never return, must be killed
dispatchResponses :: ConnectionIO ()
dispatchResponses = do
  sock <- asks connSocket
  logger <- asks connLogger
  pendingVar <- asks connPending
  let handleMessage (h, msg) =
        atomically $ do
          let sid = (frameStream h)
          pending <- readTVar pendingVar
          case M.lookup sid pending of
            Nothing
          -- XXX - handle server side events
             -> pure ()
            Just rslt -> do
              writeTVar pendingVar (M.delete sid pending)
              putTMVar rslt msg
              pure ()
  liftIO $
    runConduit
      (appSource sock .| CC.mapM (logger "response") .| conduitGet2 message .|
       CC.mapM_ handleMessage)
