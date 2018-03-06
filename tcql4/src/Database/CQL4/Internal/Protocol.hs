{- | CQL4 protocol defined in terms of cereal gets and puts -}
module Database.CQL4.Internal.Protocol where

import Control.Concurrent.QSem
import Control.Monad (replicateM)
import Control.Monad.Except (liftIO, throwError)
import Control.Monad.Reader (asks)
import Data.Conduit ((.|), runConduit)
import Data.Conduit.Cereal (conduitGet2, sourcePut)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Network (appSink, appSource)
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as M
import Data.Int (Int32)
import qualified Data.List as L
import Data.Monoid ((<>))
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import qualified Data.Text as T
import Data.Traversable (for)
import Database.CQL4.Exceptions
import qualified Database.CQL4.Internal.Get as CG
import qualified Database.CQL4.Internal.Put as CP
import Database.CQL4.Internal.Types
import Database.CQL4.Protocol
import Database.CQL4.Types
import UnliftIO.Exception (bracket_)
import UnliftIO.STM

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
queryResult (ResultMsg x) = pure x
queryResult x =
  throwError $
  messageException ("invalid response to query command: " <> show x)

-- | Get the actual result rows
--
-- Any QueryResult will, it's just an empty list if it does not have any rows
resultRows :: QueryResult -> ConnectionIO [[TypedValue]]
resultRows (QueryResultRows rr) = pure (rrRows rr)
resultRows _ = pure []

-- | Get the result rows object
--
-- This is the stricter version - only a QueryResultRows will do, anything
-- else is an error.
resultRows' :: QueryResult -> ConnectionIO ResultRows
resultRows' (QueryResultRows rr) = pure rr
resultRows' x =
  throwError $ messageException ("not a result with rows: " <> show x)

-- | Get a Unit result
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

-- | Put the startup message
--
-- This is the first request on a new connection and
-- the server will respond either with `ERROR`
-- `READY` or `AUTHENTICATE`
startup :: StreamID -> P.Put
startup sid =
  P.putNested
    (CP.frameHeader RequestFrame [] sid OpStartup)
    (CP.map CP.string (M.singleton "CQL_VERSION" "3.3.0"))

-- | Put the Options message
--
-- Get server options.  This can be issued before startup.
options :: StreamID -> P.Put
options sid = CP.frameHeader RequestFrame [] sid OpOptions 0

-- | simplified query constructor
makeQuery :: Consistency -> T.Text -> [TypedValue] -> Query
makeQuery cl cql vs =
  Query
  { queryText = cql
  , queryConsistency = cl
  , queryValues = vs
  , querySkipMetadata = False
  , queryPageSize = Nothing
  , queryPagingState = Nothing
  , querySerialConsistency = Nothing
  , queryDefaultTimestamp = Nothing
  }

putQuery :: Query -> StreamID -> P.Put
putQuery q sid =
  P.putNested (CP.frameHeader RequestFrame [] sid OpQuery) $ do
    CP.longString (queryText q)
    CP.consistency (queryConsistency q)
    CP.queryFlags q
    CP.queryValues q
    for_ (queryPageSize q) CP.int
    for_ (queryPagingState q) CP.bytes
    for_ (querySerialConsistency q) CP.consistency
    for_ (queryDefaultTimestamp q) CP.timestamp

-- | 
-- | Get the next message from the server
message :: G.Get (FrameHeader, Message)
message = do
  h <- CG.frameHeader
  G.label "message" $
    G.isolate (fromIntegral $ frameLength h) $
    case frameOpCode h of
      OpError -> (h, ) <$> errorMessage
      OpReady -> pure (h, ReadyMsg)
      OpAuthenticate -> (h, ) . AuthenticateMsg <$> CG.string
      OpSupported -> (h, ) . SupportedMsg <$> CG.strmap (CG.list CG.string)
      OpResult -> (h, ) <$> resultMessage
      x -> fail ("not implemented: " ++ show x)

resultMessage :: G.Get Message
resultMessage = do
  kind <- CG.int
  case kind of
    0x0001 -> pure $ ResultMsg QueryResultVoid
    0x0002 -> resultRowsMessage
    0x0003 -> do
      ks <- CG.string
      pure $ ResultMsg $ QueryResultKeyspace ks
    0x0005 -> schemaChangedMessage
    _ -> fail ("Unknown result message kind: " <> show kind)

resultRowsMessage :: G.Get Message
resultRowsMessage = do
  flags <- CG.metadataFlags
  colcnt <- fromIntegral <$> CG.int
  ps <-
    if HasMorePages `L.elem` flags
      then Just <$> (CG._shortLen >>= CG._blob)
      else pure Nothing
      -- XXX: when ?
  if NoMetadata `L.elem` flags
        -- would have to pass it in from some prior prepare result
        -- can't parse the result rows without knowing the format
    then fail "NoMetadata not supported"
    else pure ()
  gt <-
    if GlobalTableSpec `L.elem` flags
      then Just <$> ((,) <$> CG.string <*> CG.string)
      else pure Nothing
  colSpecs <- replicateM colcnt (CG.columnType gt)
  rowcnt <- fromIntegral <$> CG.int
  rows <- replicateM rowcnt (for colSpecs (CG.typedBytes . columnType))
  pure $
    ResultMsg $ QueryResultRows $ ResultRows colcnt gt ps colSpecs rowcnt rows

errorMessage :: G.Get Message
errorMessage = do
  code <- CG.int
  msg <- CG.string
  case code of
    0x0000 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x000A -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x0100 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x1000 -> _errorMsg code msg [_epConsistency, _epInt "req", _epInt "alive"]
    0x1001 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x1002 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x1003 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x1100 ->
      _errorMsg
        code
        msg
        [ _epConsistency
        , _epInt "received"
        , _epInt "blockFor"
        , _epString "writeType"
        ]
    0x1200 ->
      _errorMsg
        code
        msg
        [ _epConsistency
        , _epInt "received"
        , _epInt "blockFor"
        , _epBool "dataPresent"
        ]
    0x1300 ->
      _errorMsg
        code
        msg
        [ _epConsistency
        , _epInt "received"
        , _epInt "blockFor"
        , _epInt "numFailures"
        , _epBool "dataPresent"
        ]
    0x1400 ->
      _errorMsg
        code
        msg
        [ _epString "keyspace"
        , _epString "function"
        , _ep "argTypes" show' (CG.list CG.string)
        ]
    0x1500 ->
      _errorMsg
        code
        msg
        [ _epConsistency
        , _epInt "received"
        , _epInt "blockFor"
        , _epInt "numFailures"
        , _epString "writeType"
        ]
    0x2000 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x2100 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x2200 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x2300 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    0x2400 -> _errorMsg code msg [_epString "keyspace", _epString "table"]
    0x2500 -> pure $ ErrorMsg $ ErrorMsgT code msg []
    x -> pure $ ErrorMsg $ ErrorMsgT code msg [("unknown code", show' x)]
  where
    show' :: Show a => a -> T.Text
    show' = T.pack . show
    _errorMsg :: Int32 -> T.Text -> [G.Get (T.Text, T.Text)] -> G.Get Message
    _errorMsg code msg ps = ErrorMsg . ErrorMsgT code msg <$> sequence ps
    _ep :: T.Text -> (a -> T.Text) -> G.Get a -> G.Get (T.Text, T.Text)
    _ep n f g = ((n, ) . f) <$> g
    _epConsistency :: G.Get (T.Text, T.Text)
    _epConsistency = _ep "cl" show' CG.consistency
    _epInt :: T.Text -> G.Get (T.Text, T.Text)
    _epInt n = _ep n show' CG.int
    _epString :: T.Text -> G.Get (T.Text, T.Text)
    _epString n = (n, ) <$> CG.string
    _epBool :: T.Text -> G.Get (T.Text, T.Text)
    _epBool n = _ep n show' CG.bool

schemaChangedMessage :: G.Get Message
schemaChangedMessage = do
  typStr <- CG.string
  typ <-
    case typStr of
      "CREATED" -> pure ChangeCreated
      "UPDATED" -> pure ChangeUpdated
      "DROPPED" -> pure ChangeDropped
      _ -> fail ("Unknown change type: " <> T.unpack typStr)
  target <- CG.string
  sc <-
    case target of
      "KEYSPACE" -> SchemaChange typ . ChangeKeyspace <$> CG.string
      "TABLE" -> sc2 typ ChangeTable
      "TYPE" -> sc2 typ ChangeType
      "FUNCTION" -> sc3 typ ChangeFunction
      "AGGREGATE" -> sc3 typ ChangeAggregate
      _ -> fail ("Unknown schema change target: " <> T.unpack target)
  pure $ ResultMsg $ QueryResultSchemaChanged sc
  where
    sc2 t f = SchemaChange t <$> (f <$> CG.string <*> CG.string)
    sc3 t f =
      SchemaChange t <$> (f <$> CG.string <*> CG.string <*> CG.list CG.string)
