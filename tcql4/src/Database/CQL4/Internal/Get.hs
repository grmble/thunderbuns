module Database.CQL4.Internal.Get where

import Control.Monad (replicateM)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M
import Data.Int (Int16, Int32, Int64)
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
import Data.Word (Word16, Word32)
import qualified Database.CQL4.Internal.GetVarint as GV
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

_value :: (Int -> G.Get a) -> Int -> G.Get (Value a)
_value g n =
  case n of
    -1 -> pure NullValue
    -2 -> pure NotSet
    x
      | x >= 0 -> Value <$> g x
    _ -> fail ("illegal len for value: " <> show n)

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

newtype UUID =
  UUID B.ByteString

-- | a 16 bytes long uuid
uuid :: G.Get UUID
uuid = UUID <$> _blob 16

-- | a list of n elements
--
-- it is called list in the cassandra protocoll definitions
list :: G.Get a -> G.Get [a]
list g = do
  len <- _shortLen
  replicateM len g

-- | an int n, followed by n bytes if n >= 0
bytes :: G.Get (Maybe B.ByteString)
bytes = _intLen >>= _maybe _blob

-- | an int n, followed by n bytes if n >= 0.  -1 null, -2 not set
value :: G.Get (Value B.ByteString)
value = _intLen >>= _value _blob

-- | a short n, followed by n bytes if n >= 0
--
-- XXX: duh? short is defined as unsigned?  I assume here it is signed
shortBytes :: G.Get (Maybe B.ByteString)
shortBytes = _signedShortLen >>= _maybe _blob

-- | a pair of <id><value> where id is a short
option :: (Word16 -> G.Get a) -> G.Get (Word16, a)
option f = do
  w <- G.getWord16be
  a <- f w
  pure (w, a)

-- | a short n followed by n key/value pairs. key is always string.
-- * string map: map string
-- * string multimap: map (list string)
-- * bytes map: map bytes
map :: G.Get a -> G.Get (M.HashMap T.Text a)
map g = do
  n <- _shortLen
  M.fromList <$> replicateM n ((,) <$> string <*> g)

-- | a single byte, 0 denotes false, everything else is true
boolean :: G.Get Bool
boolean = do
  b <- G.getWord8
  pure $ b == 0

-- | an unsigned integer representing days with epoch centered at 2^31
date :: G.Get Day
date = do
  d <- G.getInt32be
  pure $ addDays (fromIntegral d) _epochDate

-- | int scale followed by varint value (unscaled)E(-scale)
decimal :: G.Get Scientific.Scientific
decimal = do
  scale <- int
  unscaled <- varint
  pure $ Scientific.scientific (fromIntegral unscaled) (fromIntegral $ -scale)

-- a 8-byte floating point number in the ieee 754 format
double :: G.Get Double
double = getFloat64be

-- a 4-byte floating point number in the ieee 754 format
float :: G.Get Float
float = getFloat32be

-- | A ip address
data IPAddress
  = IPAddressV4 Word32
  | IPAddressV6 (Word32, Word32, Word32, Word32)

-- | a 4-byte or 16-byte sequence denoting an IP4 or IPv6 address
ipAddress :: G.Get IPAddress
ipAddress = do
  len <- _byteLen
  case len of
    4 -> IPAddressV4 <$> G.getWord32be
    16 ->
      IPAddressV6 <$>
      ((,,,) <$> G.getWord32be <*> G.getWord32be <*> G.getWord32be <*>
       G.getWord32be)
    _ -> fail "IP address must be 4 or 16 bytes"

-- | an ip address followed by an int port
inet :: G.Get (IPAddress, Int32)
inet = (,) <$> ipAddress <*> int

-- | a short giving the consistency
consistency :: G.Get Consistency
consistency = toEnum <$> _shortLen

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
varint :: G.Get Int64
varint = GV.varint

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
    _ct 0x0004 = pure CTBoolean
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
