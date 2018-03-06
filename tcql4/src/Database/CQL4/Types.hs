{- | CQL4 API Types

Types used in the externally visible API.

This can be imported from the internal modules.
So this file only has the most basic types.

Protocol.hs has the types that need the internal modules.
-}
module Database.CQL4.Types where

import qualified Data.ByteString as B
import Data.Int
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime)
import Data.UUID (UUID)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)

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

-- | Result Rows in a query result
data ResultRows = ResultRows
  { rrColumnsCount :: Int
  , rrGlobalTableSpec :: Maybe (T.Text, T.Text)
  , rrPagingState :: Maybe B.ByteString
  , rrColumns :: V.Vector ColumnSpec
  , rrCount :: Int
  , rrRows :: [V.Vector TypedValue]
  } deriving (Show, Eq)

-- | A column spec describes a column
data ColumnSpec = ColumnSpec
  { tableSpec :: (T.Text, T.Text)
  , columnName :: T.Text
  , columnType :: ColumnType
  } deriving (Show, Eq)

-- | The possible types of a column
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
