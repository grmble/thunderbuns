{-# LANGUAGE StrictData #-}

{- | CQL4 Connection

Basically, this is a socket to a Cassandra/ScyllaDB server.

Commands are sent over the socket (with a generated ID), response
messages are received over the socket.  The protocol is fully
asynchronous - messages can come in in any order.  They do contain
the ID of their command.

So the connection abstraction also allows to wait for the response
to a command.

The module distinguished protocol errors and user errors.
Both are of type CQLException, but protocoll errors will be thrown.
This is because protocol errors will leave the socket unusable, e.g
a failing handshare. User errors can be recovered - e.g. an error message
for a cql statement with an error.

By using `runConnection` instead of `runConnection`, all errors
will be thrown as exceptions.
-}
module Database.CQL4.Connection
  ( ConnectionIO
  , Connection(..)
  , connection
  , connection'
  , runConnection
  , runConnection'
  , hexdumpLogger
  , closeConnection
  , command
  , queryResult
  , unitResult
  , resultRows
  ) where

import Control.Concurrent.QSem
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, asks, liftIO, runReaderT)
import qualified Data.ByteString as B
import Data.ByteString.Hexdump (hexdump)
import qualified Data.ByteString.Lazy as LB
import Data.Conduit ((.|), runConduit)
import Data.Conduit.Cereal (conduitGet2, sourcePut)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Network (AppData, appSink, appSource)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import qualified Data.Serialize.Put as P
import Data.String (fromString)
import Database.CQL4.Exceptions
import Database.CQL4.Internal.Protocol
import Database.CQL4.Internal.Types
import UnliftIO.Concurrent (ThreadId, forkIO, killThread)
import UnliftIO.Exception (bracket_, onException, throwIO)
import UnliftIO.STM

-- | A Connection is a ReaderT with mutable state + ExceptT
type ConnectionIO = ReaderT Connection (ExceptT CQLException IO)

-- | Execute the ConnectionIO monad
runConnection :: ConnectionIO a -> Connection -> IO (Either CQLException a)
runConnection m c = runExceptT $ runReaderT m c

-- | Execute the ConnectionIO monad, throwing an exception on error
runConnection' :: ConnectionIO a -> Connection -> IO a
runConnection' m c = do
  ea <- runConnection m c
  case ea of
    Left ex -> throwIO ex
    Right a -> pure a

-- | The data inside a ConnectionIO ReaderT
data Connection = Connection
  { connSocket :: AppData -- ^ conduit app data for the underlying socket
  , connSem :: QSem -- ^ semaphore that guards writing to the socket
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

-- | Connection constructor for use with runReaderT
--
-- The connection is completely set up after this -
-- the CQL handshake has taken place, everything is ready.
--
-- Note that this allocates resources, you should bracket
-- this call with `closeConnection`.
connection :: AppData -> IO Connection
connection = connection' (const pure)

-- | Connection constructor that takes a logger.
--
-- Choices are `hexdumpLogger` or `(const pure)`
connection' :: Logger -> AppData -> IO Connection
connection' logger sock = do
  idVar <- newTVarIO 0
  pVar <- newTVarIO M.empty
  dVar <- newEmptyTMVarIO
  qsem <- newQSem 1
  let conn = Connection sock qsem idVar pVar dVar logger
  onException
    (runConnection' initConnection conn)
    (runConnection' closeConnection conn)

-- | Log a hexdump of the request/response
hexdumpLogger :: Logger
hexdumpLogger msg bs = do
  LB.putStr
    (fromString msg `LB.append` "\n" `LB.append` hexdump (LB.fromStrict bs))
  pure bs

-- | Initialize the connection.
--
-- Handles the CQL4 initialization (e.g. startup message, auth, ...)
-- and will block until this is completed.
--
-- After this initialization is complete, it is safe to use the
-- connection from multiple threads.
--
-- This will throw an error if there is a protocol problem
-- (e.g. if the server wants authentication).
initConnection :: ConnectionIO Connection
initConnection = do
  cdata <- ask
  tid <- liftIO $ forkIO $ runConnection' dispatchResponses cdata
  liftIO $ atomically $ putTMVar (connDispatcher cdata) tid
  msg <- command startup
  case msg of
    ReadyMsg -> pure cdata
    x -> throwIO $ messageException "unexpected response type" x

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
  p <- liftIO newEmptyTMVarIO
  let go pending sid =
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
  (_, rslt) <- command' cmd
  atomically (takeTMVar rslt)

-- | Send a command frame over the socket, but do not wait.
--
-- It returns the TMVar instead that can be waited on.
command' :: (StreamID -> P.Put) -> ConnectionIO (StreamID, TMVar Message)
command' cmd = do
  sock <- asks connSocket
  logger <- asks connLogger
  sem <- asks connSem
  (sid, rslt) <- streamID
  liftIO $
    bracket_ (waitQSem sem) (signalQSem sem) $
    runConduit
      (sourcePut (cmd sid) .| CC.mapM (logger "request") .| appSink sock)
  pure (sid, rslt)

-- | Get the QueryResult from the message.
--
-- Any other message type is an error.
queryResult :: Message -> ConnectionIO QueryResult
queryResult msg =
  case msg of
    ResultMsg x -> pure x
    x -> throwError $ messageException "invalid response to query command" x

-- | Get the actual result rows
--
-- Used as `command .... >>= queryResult >>= resultRows`
resultRows :: QueryResult -> ConnectionIO [[TypedValue]]
resultRows = pure . _resultRows

unitResult :: Message -> ConnectionIO ()
unitResult msg = queryResult msg $> ()

-- ! Dispatch server respones server responses - will never return, must be killed
--
-- XXX; on exception, when bugging out, should set this in the connection.
-- all pendings waits should be errored, all future queries should error immediately
dispatchResponses :: ConnectionIO ()
dispatchResponses = do
  sock <- asks connSocket
  logger <- asks connLogger
  pendingVar <- asks connPending
  let handleMessage (h, msg) =
        atomically $ do
          let sid = frameStream h
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
