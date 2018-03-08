module Database.CQL4.Internal.Get where

import Control.Monad (replicateM)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
import Data.Int (Int16, Int32, Int64)
import qualified Data.List as L
import Data.Monoid ((<>))
import qualified Data.Scientific as Scientific
import qualified Data.Serialize.Get as G
import Data.Serialize.IEEE754 (getFloat32be, getFloat64be)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Clock
  ( DiffTime
  , UTCTime(..)
  , addUTCTime
  , picosecondsToDiffTime
  )
import Data.Traversable (for)
import qualified Data.UUID as U
import Data.Word (Word16)
import Database.CQL4.Internal.Types
import Database.CQL4.Types

frameHeader :: G.Get FrameHeader
frameHeader = do
  v' <- G.getWord8
  v <-
    case v' of
      0x4 -> pure RequestFrame
      0x84 -> pure ResponseFrame
      _ -> fail "frameVersion not 0x04/0x84"
  f' <- G.getWord8
  let fs = foldr (foldFlags f') [] (fromEnum <$> [FlagCompress ..])
  s' <- G.getInt16be
  c' <- G.getWord8
  l' <- G.getInt32be
  pure
    FrameHeader
    { frameVersion = v
    , frameFlags = fs
    , frameStream = s'
    , frameOpCode = toEnum $ fromIntegral c'
    , frameLength = l'
    }
  where
    foldFlags f' e acc =
      if testBit f' e
        then toEnum e : acc
        else acc

--
--
-- helpers
--
--
_intLen :: G.Get Int
_intLen = fromIntegral <$> int

_shortLen :: G.Get Int
_shortLen = fromIntegral <$> short

_signedShortLen :: G.Get Int
_signedShortLen = fromIntegral <$> signedShort

_byteLen :: G.Get Int
_byteLen = fromIntegral <$> G.getWord8

_epochDate :: Day
_epochDate = fromGregorian 1970 01 01

_epochTimestamp :: UTCTime
_epochTimestamp = UTCTime _epochDate 0

-- | a blob of n bytes
_blob :: Int -> G.Get B.ByteString
_blob = G.getBytes

_maybe :: (Int -> G.Get a) -> Int -> G.Get (Maybe a)
_maybe g n =
  if n < 0
    then pure Nothing
    else Just <$> g n

-- | utf8 decode the bytestring or fail
_utf8 :: B.ByteString -> G.Get T.Text
_utf8 bs =
  case TE.decodeUtf8' bs of
    Left ex -> fail $ show ex
    Right txt -> pure txt

-- | a utf-8 encoded string of n bytes
_string :: Int -> G.Get T.Text
_string n = _blob n >>= _utf8

--
--
--  primitives from the cassandra protocol spec
--
--
-- | a 4-byte two's complement integer
int :: G.Get Int32
int = G.getInt32be

-- | an eight-byte two's complement integer
long :: G.Get Int64
long = G.getInt64be

-- | a 2 byte unsigned integer
short :: G.Get Word16
short = G.getWord16be

-- | a 2 byte two's complement integer
signedShort :: G.Get Int16
signedShort = G.getInt16be

-- | a short n, followed by n bytes representing an UTF-8 string
string :: G.Get T.Text
string = _shortLen >>= _string

-- | an int n, followed by n bytes representing an utf-8 string
longString :: G.Get T.Text
longString = _intLen >>= _string

uuid :: G.Get U.UUID
uuid = do
  bs <- _blob 16
  case U.fromByteString (LB.fromStrict bs) of
    Nothing -> fail "UUID.fromByteString"
    Just x -> pure x

-- | a list of n elements
--
-- it is called list in the cassandra protocoll definitions
list :: G.Get a -> G.Get [a]
list g = do
  len <- _shortLen
  replicateM len g

-- | a list with int len
--
-- this is the kind of list in the results
-- i assume whereever they write byte, you have an int len ...
bytesList :: G.Get a -> G.Get [a]
bytesList g = do
  len <- _intLen
  replicateM len g

-- | a short n, followed by n bytes if n >= 0
shortBytes :: G.Get (Maybe B.ByteString)
shortBytes = _signedShortLen >>= _maybe _blob

-- | a pair of <id><value> where id is a short
option :: (Word16 -> G.Get a) -> G.Get (Word16, a)
option f = do
  w <- G.getWord16be
  a <- f w
  pure (w, a)

-- | an int n followed by n key/value pairs.
--
-- this is the kind of map that shows up in result sets
bytesMap :: G.Get a -> G.Get b -> G.Get [(a, b)]
bytesMap k v = do
  n <- _intLen
  replicateM n ((,) <$> k <*> v)

--- | a short n followed by n key/value pairs. key is always string.
--
-- this is the kind of map returned by options (or other places where the
-- types are fixed)
--
-- * string map: map string
-- * string multimap: map (list string)
-- * bytes map: map bytes
strmap :: G.Get a -> G.Get (M.HashMap T.Text a)
strmap g = do
  n <- _shortLen
  M.fromList <$> replicateM n ((,) <$> string <*> g)

-- | a single byte, 0 denotes false, everything else is true
bool :: G.Get Bool
bool = do
  b <- G.getWord8
  pure $ b /= 0

-- | an unsigned integer representing days with epoch centered at 2^31
date :: G.Get Day
date = do
  d <- G.getInt32be
  pure $ addDays (fromIntegral d) _epochDate

-- | int scale followed by varint value (unscaled)E(-scale)
decimal :: Int -> G.Get Scientific.Scientific
decimal n = do
  scale <- int
  unscaled <- varint (n - 4)
  pure $ Scientific.scientific unscaled (fromIntegral $ -scale)

-- a 8-byte floating point number in the ieee 754 format
double :: G.Get Double
double = getFloat64be

-- a 4-byte floating point number in the ieee 754 format
float :: G.Get Float
float = getFloat32be

-- | a 4-byte or 16-byte sequence denoting an IP4 or IPv6 address
ipAddress :: G.Get IPAddress
ipAddress = do
  len <- G.remaining
  case len of
    4 -> IPAddressV4 <$> G.getWord32be
    16 ->
      IPAddressV6 <$>
      ((,,,) <$> G.getWord32be <*> G.getWord32be <*> G.getWord32be <*>
       G.getWord32be)
    _ -> fail ("IP address must be 4 or 16 bytes: " <> show len)

-- | an ip address followed by an int port
--
-- protocol definition says inet is ipaddress/port, but it seems
-- you only get an address from such a db field
inet :: G.Get (IPAddress, Int32)
inet = (,) <$> ipAddress <*> int

-- | a short giving the consistency
consistency :: G.Get Consistency
consistency = toEnum <$> _shortLen

-- | a short encoding metadata flags
metadataFlags :: G.Get [MetadataFlag]
metadataFlags = do
  flag <- int
  pure $ foldr (foldFlags flag) [] (fromEnum <$> [FlagCompress ..])
  where
    foldFlags f' e acc =
      if testBit f' e
        then toEnum e : acc
        else acc

-- | an 8 byte two's complement long representing nanoseconds since midnight
time :: G.Get DiffTime
time = do
  ns <- G.getInt64be
  pure $ picosecondsToDiffTime (fromIntegral ns * 1000)

-- | an 8 byte two's complement integer representing milliseconds since epoch
timestamp :: G.Get UTCTime
timestamp = do
  ms <- G.getInt64be
  pure $ addUTCTime (realToFrac ms / 1000) _epochTimestamp

-- | variable length two's complement encoding of signed integer
--
-- this is when reading a result value - we know the length.
-- it's just the given number of bytes in big endian order.
-- when the first byte has the MSB set, it's a negative number.
--
-- this is different from cassandra vint coding - which is what
-- i tried to use at first.
varint :: Int -> G.Get Integer
varint n = do
  i0 <- G.getInt8
  is <- replicateM (n - 1) G.getInt8
  pure $ L.foldl' go (fromIntegral i0) is
  where
    go acc i = (acc `shiftL` 8) .|. (fromIntegral i .&. 0xFF)

-- | get a column type
columnType :: Maybe (T.Text, T.Text) -> G.Get ColumnSpec
columnType gts = do
  tspec <-
    case gts of
      Just t' -> pure t'
      Nothing -> (,) <$> string <*> string
  name <- string
  ct <- _columnType
  pure $ ColumnSpec tspec name ct

_columnType :: G.Get ColumnType
_columnType = snd <$> option _ct
  where
    _ct :: Word16 -> G.Get ColumnType
    _ct 0x0000 = CTCustom <$> string
    _ct 0x0001 = pure CTAscii
    _ct 0x0002 = pure CTBigint
    _ct 0x0003 = pure CTBlob
    _ct 0x0004 = pure CTBool
    _ct 0x0005 = pure CTCounter
    _ct 0x0006 = pure CTDecimal
    _ct 0x0007 = pure CTDouble
    _ct 0x0008 = pure CTFloat
    _ct 0x0009 = pure CTInt
    _ct 0x000B = pure CTTimestamp
    _ct 0x000C = pure CTUuid
    _ct 0x000D = pure CTVarchar
    _ct 0x000E = pure CTVarint
    _ct 0x000F = pure CTTimeuuid
    _ct 0x0010 = pure CTInet
    _ct 0x0011 = pure CTDate
    _ct 0x0012 = pure CTTime
    _ct 0x0013 = pure CTSmallint
    _ct 0x0014 = pure CTTinyint
    _ct 0x0020 = CTList <$> _columnType
    _ct 0x0021 = CTMap <$> _columnType <*> _columnType
    _ct 0x0022 = CTSet <$> _columnType
    _ct 0x0030 = do
      ks <- string
      udt <- string
      n <- _shortLen
      tups <- replicateM n ((,) <$> string <*> _columnType)
      pure $ CTUDT ks udt tups
    _ct 0x0031 = do
      n <- _shortLen
      CTTuple <$> replicateM n _columnType
    _ct x = fail ("unknown column type: " ++ show x)

-- | an int n followed by n bytes representing the datatype
--
-- the spec talks of `bytes` and `values`.  `bytes` have a null value
-- encoded by negative length.  also an `empty` value encoded by 0 length.
--
-- `values` are used when sending to the server: here -1 means null,
-- -2 means `NotSet`
typedBytes :: ColumnType -> G.Get TypedValue
typedBytes ct = do
  len <- _intLen
  if len < 0
    then pure NullValue
    else case ct of
           CTCustom n -> CustomValue n <$> go "custom" len "" (_blob len)
           CTAscii -> TextValue <$> go "ascii" len "" (_string len)
           CTBigint -> LongValue <$> go "bigint" len 0 long
           CTBlob -> BlobValue <$> go "blob" len "" (_blob len)
           CTBool -> BoolValue <$> go "bool" len False bool
           CTCounter -> LongValue <$> go "counter" len 0 long
           CTDecimal -> DecimalValue <$> go "decimal" len 0 (decimal len)
           CTDouble -> DoubleValue <$> go "double" len 0 double
           CTFloat -> FloatValue <$> go "float" len 0 float
           CTInt -> IntValue <$> go "int" len 0 int
           CTTimestamp ->
             TimestampValue <$> go "timestamp" len _epochTimestamp timestamp
           CTUuid -> UUIDValue <$> gofail len "empty.uuid" uuid
           CTVarchar -> TextValue <$> go "varchar" len "" (_string len)
           CTVarint -> VarintValue <$> go "varint" len 0 (varint len)
           CTTimeuuid -> TimeUUIDValue <$> gofail len "empty.timeuuid" uuid
           CTInet -> InetValue <$> gofail len "empty.inet.address" ipAddress
           CTDate -> DateValue <$> go "date" len _epochDate date
           CTTime -> TimeValue <$> go "time" len 0 time
           CTSmallint -> SmallintValue <$> go "smallint" len 0 signedShort
           CTTinyint -> TinyintValue <$> go "tinyint" len 0 G.getInt8
           CTList ct' ->
             ListValue <$> go "list" len [] (bytesList $ typedBytes ct')
           CTMap ctk ctv ->
             MapValue <$>
             go "map" len [] (bytesMap (typedBytes ctk) (typedBytes ctv))
           CTSet ct' ->
             SetValue <$> go "set" len [] (bytesList $ typedBytes ct')
           -- XXX CTUDT ks un cts -> undefined
           CTTuple cts -> TupleValue <$> go "tuple" len [] (for cts typedBytes)
  where
    go :: String -> Int -> a -> G.Get a -> G.Get a
    go msg len e g =
      if len == 0
        then pure e
        else G.label msg (G.isolate len g)
    gofail :: Int -> String -> G.Get a -> G.Get a
    gofail len msg g =
      if len == 0
        then fail msg
        else G.isolate len g
