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
module Database.CQL4.Connection where

import Control.Concurrent.QSem
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, asks, liftIO, runReaderT)
import Data.ByteString.Hexdump (hexdump)
import qualified Data.ByteString.Lazy as LB
import Data.Conduit.Network (AppData)
import qualified Data.HashMap.Strict as M
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.CQL4.Exceptions
import Database.CQL4.Internal.Protocol
import Database.CQL4.Internal.Types
import Database.CQL4.Protocol
import Database.CQL4.Types
import UnliftIO.Concurrent (forkIO, killThread)
import UnliftIO.Exception (onException, throwIO)
import UnliftIO.STM

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
    x -> throwIO $ messageException ("unexpected response type: " ++ show x)

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

-- | Execute a CQL query when you are not interested in the result.
execute :: Consistency -> T.Text -> [TypedValue] -> ConnectionIO ()
execute cl cql vs = do
  msg <- command (putQuery $ makeQuery cl cql vs)
  unitResult msg

-- | Execute a CQL query and return the results
executeQuery ::
     Consistency -> T.Text -> [TypedValue] -> ConnectionIO [V.Vector TypedValue]
executeQuery cl cql vs = do
  msg <- command (putQuery $ makeQuery cl cql vs)
  queryResult msg >>= resultRows
