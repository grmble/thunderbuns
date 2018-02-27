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

-- | A value can have a value, it can be null or unset.
data Value a
  = Value a
  | NullValue
  | NotSet
  deriving (Show, Eq)

-- | A CQL query
--
-- Note that this does not have query flags - the query flags
-- are determined by the type of the query (and the parameters
-- that are present)
data Query = UnboundQuery -- ^ a query without bound variables
  { query :: T.Text
  , consistency :: Consistency
  , skipMetadata :: Bool
  , pageSize :: Maybe Int32
  , pagingState :: Maybe B.ByteString
  , serialConsistency :: Maybe Consistency
  , defaultTimestamp :: Maybe UTCTime
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

data QueryResult
  = QueryResultVoid
  | QueryResultRows { columnsCount :: Int
                    , globalTableSpec :: Maybe (T.Text, T.Text)
                    , columns :: [ColumnSpec]
                    , rowsCount :: Int
                    , rows :: [TypedValue] }
  | QueryResultKeyspace T.Text
  | QueryResultPrepared -- XXX implement me
  | QueryResultSchemaChanged -- XXX implement me
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
  | CTBoolean
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

data TypedValue
  = TextValue T.Text
  | LongValue Int64
  | IntValue Int32
  | ShortValue Int16
  | TinyValue Int8
  | DecimalValue Scientific.Scientific
  | BoolValue Bool
  | BlobValue B.ByteString
  | TimestampValue UTCTime
  | DateValue Day
  | TimeValue DiffTime
  | UUIDValue B.ByteString
  | TimeUUIDValue B.ByteString
  deriving (Show, Eq)
