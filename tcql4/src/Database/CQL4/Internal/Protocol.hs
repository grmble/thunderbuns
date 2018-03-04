{- | CQL4 protocol defined in terms of cereal gets and puts -}
module Database.CQL4.Internal.Protocol
  ( startup
  , options
  , makeQuery
  , executeQuery
  , message
  ) where

import Control.Monad (replicateM)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import Data.Int (Int32)
import qualified Data.List as L
import Data.Monoid ((<>))
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Database.CQL4.Internal.Get as CG
import qualified Database.CQL4.Internal.Put as CP
import Database.CQL4.Types

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

executeQuery :: Query -> StreamID -> P.Put
executeQuery q sid =
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
  pure $ ResultMsg $ QueryResultRows colcnt gt ps colSpecs rowcnt rows

errorMessage :: G.Get Message
errorMessage = do
  code <- CG.int
  msg <- CG.string
  case code of
    0x0000 -> pure $ ErrorMsg code msg []
    0x000A -> pure $ ErrorMsg code msg []
    0x0100 -> pure $ ErrorMsg code msg []
    0x1000 -> _errorMsg code msg [_epConsistency, _epInt "req", _epInt "alive"]
    0x1001 -> pure $ ErrorMsg code msg []
    0x1002 -> pure $ ErrorMsg code msg []
    0x1003 -> pure $ ErrorMsg code msg []
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
    0x2000 -> pure $ ErrorMsg code msg []
    0x2100 -> pure $ ErrorMsg code msg []
    0x2200 -> pure $ ErrorMsg code msg []
    0x2300 -> pure $ ErrorMsg code msg []
    0x2400 -> _errorMsg code msg [_epString "keyspace", _epString "table"]
    0x2500 -> pure $ ErrorMsg code msg []
    x -> pure $ ErrorMsg code msg [("unknown code", show' x)]
  where
    show' :: Show a => a -> T.Text
    show' = T.pack . show
    _errorMsg :: Int32 -> T.Text -> [G.Get (T.Text, T.Text)] -> G.Get Message
    _errorMsg code msg ps = ErrorMsg code msg <$> sequence ps
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
