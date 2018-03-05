{-# LANGUAGE StrictData #-}

{- | CQL Types common for get and put

Note that the values of the Enums are highly magical -
they are the actual codes on the wire.

No rearranging ...
-}
module Database.CQL4.Types where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M
import Data.Int
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime)
import Data.UUID (UUID)
import Data.Word (Word32, Word64)

-- | The frame header is transmitted before the data of the frame
data FrameHeader = FrameHeader
  { frameVersion :: FrameVersion
  , frameFlags :: [FrameFlag]
  , frameStream :: StreamID
  , frameOpCode :: OpCode
  , frameLength :: Int32 -- ^ Length of the following frame body, max 256MB
  } deriving (Show, Eq)

-- | A stream id identifies the request of a response
--
-- The client must not generate negative stream ids (reserved for events
-- originating from the server)
type StreamID = Int16

-- | The FrameVersion is either 0x04 (Request) or 0x84 (Response)
data FrameVersion
  = RequestFrame
  | ResponseFrame
  deriving (Show, Eq)

-- | The flags are combinined in a single Word8
--
-- The enum gives the bit position - toggle the bit to set the flag
data FrameFlag
  = FlagCompress
  | FlagTrace
  | FlagCustomPayload
  | FlagWarning
  deriving (Show, Eq, Enum)

-- | Frame Op Codes
data OpCode
  = OpError
  | OpStartup
  | OpReady
  | OpAuthenticate
  | OpUnusedV4
  | OpOptions
  | OpSupported
  | OpQuery
  | OpResult
  | OpPrepare
  | OpExecute
  | OpRegister
  | OpEvent
  | OpBatch
  | OpAuthChallenge
  | OpAuthResponse
  | OpAuthSuccess
  deriving (Show, Eq, Enum)

-- | Cassandra consistency level
data Consistency
  = Any
  | One
  | Two
  | Three
  | Quorum
  | All
  | LocalQuorum
  | EachQuorum
  | Serial
  | LocalSerial
  | LocalOne
  deriving (Show, Eq, Enum)

-- | A CQL query
--
-- Note that this does not have query flags - the query flags
-- are determined by the type of the query (and the parameters
-- that are present)
data Query = Query
  { queryText :: T.Text
  , queryConsistency :: Consistency
  , queryValues :: [TypedValue]
  , querySkipMetadata :: Bool
  , queryPageSize :: Maybe Int32
  , queryPagingState :: Maybe B.ByteString
  , querySerialConsistency :: Maybe Consistency
  , queryDefaultTimestamp :: Maybe UTCTime
  } deriving (Show, Eq)

-- | Query flags
--
-- Note that for this enum the enum value is the bit position,
-- the flag is 2^enum 
data QueryFlags
  = QFValues
  | QFSkipMetadata
  | QFPageSize
  | QFPagingState
  | QFSerialConsistency
  | QFDefaultTimestamp
  | QFNamedValues
  deriving (Show, Eq, Enum)

-- | Metadata flags
--
-- again, bit position enum
data MetadataFlag
  = GlobalTableSpec
  | HasMorePages
  | NoMetadata
  deriving (Show, Eq, Enum)

-- | Messages that can be received from the server
--
-- Only messages that can be received by the client are relevant here
data Message
  = ErrorMsg { errorCode :: Int32
             , errorMsg :: T.Text
             , errorParams :: [(T.Text, T.Text)] }
  | ReadyMsg
  | AuthenticateMsg T.Text
  | SupportedMsg (M.HashMap T.Text [T.Text])
  | ResultMsg QueryResult
  deriving (Show, Eq)

-- | QueryResult represents the different result types for a query
data QueryResult
  = QueryResultVoid -- ^ query with empty/no result
  | QueryResultRows { _resultColumnsCount :: Int
                    , _resultGlobalTableSpec :: Maybe (T.Text, T.Text)
                    , _resultPagingState :: Maybe B.ByteString
                    , _resultColumns :: [ColumnSpec]
                    , _resultRowsCount :: Int
                    , _resultRows :: [[TypedValue]] }
  | QueryResultKeyspace T.Text
  | QueryResultPrepared -- XXX implement me
  | QueryResultSchemaChanged SchemaChange
  deriving (Show, Eq)

data ColumnSpec = ColumnSpec
  { tableSpec :: (T.Text, T.Text)
  , columnName :: T.Text
  , columnType :: ColumnType
  } deriving (Show, Eq)

-- to think that i could have started some other project. any other project
data ColumnType
  = CTCustom T.Text
  | CTAscii
  | CTBigint
  | CTBlob
  | CTBool
  | CTCounter
  | CTDecimal
  | CTDouble
  | CTFloat
  | CTInt
  | CTTimestamp
  | CTUuid
  | CTVarchar
  | CTVarint
  | CTTimeuuid
  | CTInet
  | CTDate
  | CTTime
  | CTSmallint
  | CTTinyint
  | CTList ColumnType
  | CTMap ColumnType
          ColumnType
  | CTSet ColumnType
  | CTUDT T.Text
          T.Text
          [(T.Text, ColumnType)]
  | CTTuple [ColumnType]
  deriving (Show, Eq)

-- | A typed value in a query result
--
-- regarding the bytes/value confusion from the spec:
-- `bytes` are received in query results. they have an `int` length,
-- negative values mean `NULL`.  0 length means empty whatever that means.
-- I assume an empty string, false boolean, epoch date, 0 integer, ...
--
-- `values` are SENT to the server, here -1 means NULL, -2 means NOT SET.
-- in the intereset of not having to redefine all this:  any value can be null.
-- if `NotSet` is required, the `TypedValue` is wrapped in a `Maybe`,
-- Nothing means not set, NullValue means null.
data TypedValue
  = NullValue -- ^ null value
  | CustomValue T.Text
                B.ByteString -- ^ the custom type and the value as unprocessed bytes
  | TextValue T.Text
  | LongValue Int64
  | BoolValue Bool
  | BlobValue B.ByteString
  | CounterValue Word64
  | DecimalValue Scientific.Scientific
  | DoubleValue Double
  | FloatValue Float
  | IntValue Int32
  | TimestampValue UTCTime
  | UUIDValue UUID
  -- VarcharValue is like ascii value (we always decode utf-8, it words for ascii)
  -- VarcharValue T.Text
  -- varint decodes to Int64, just like LongValue
  | VarintValue Integer
  | TimeUUIDValue UUID
  -- XXX protocol spec says Inet is IPAddress + Port, but it seems it's only the address
  -- | InetValue (IPAddress, Int32)
  | InetValue IPAddress
  | DateValue Day
  | TimeValue DiffTime
  | SmallintValue Int16
  | TinyintValue Int8
  | ListValue [TypedValue]
  | MapValue [(TypedValue, TypedValue)]
  | SetValue [TypedValue]
  | UDTValue T.Text
             T.Text
             [(T.Text, TypedValue)]
  | TupleValue [TypedValue]
  deriving (Show, Eq)

-- | A ip address
data IPAddress
  = IPAddressV4 Word32
  | IPAddressV6 (Word32, Word32, Word32, Word32)
  deriving (Show, Eq)

-- | A schema change result or event
data SchemaChange = SchemaChange
  { changeType :: ChangeType
  , changeObject :: ChangeObject
  } deriving (Show, Eq)

-- | Change type - what happened?
data ChangeType
  = ChangeCreated
  | ChangeUpdated
  | ChangeDropped
  deriving (Enum, Show, Eq)

-- | The changed object in a schema change
--
-- Combines change_target and change_options from the protocl spec.
-- One value is the keyspace, 2 the keyspace and object, if there is a list
-- it's one string for each argument
data ChangeObject
  = ChangeKeyspace T.Text
  | ChangeTable T.Text
                T.Text
  | ChangeType T.Text
               T.Text
  | ChangeFunction T.Text
                   T.Text
                   [T.Text]
  | ChangeAggregate T.Text
                    T.Text
                    [T.Text]
  deriving (Show, Eq)
