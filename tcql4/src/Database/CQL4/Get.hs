module Thunderbuns.CQL4.Get where

import Control.Monad (replicateM)
import Data.Bits (shift)
import qualified Data.ByteString as B
import Data.Int (Int16, Int32, Int64)
import Data.Monoid ((<>))
import Data.Word (Word16)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Scientific as Scientific
import qualified Data.Serialize.Get as G
import Data.Serialize.IEEE754 (getFloat32be, getFloat64be)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Clock (DiffTime, UTCTime(..), addUTCTime, picosecondsToDiffTime)
import qualified Data.Vector as V
import Data.Word (Word32)


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
_blob n =
  G.getBytes n

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
    x | x >= 0 -> Value <$> g x
    _ -> fail ("illegal len for value: " <> show n)

-- | utf8 decode the bytestring or fail
_utf8 :: B.ByteString -> G.Get T.Text
_utf8 bs =
  case TE.decodeUtf8' bs of
    Left ex -> fail $ show ex
    Right txt -> pure txt

-- | a utf-8 encoded string of n bytes
_string :: Int -> G.Get T.Text
_string n =
  _blob n >>= _utf8


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
long =
  G.getInt64be

-- | a 2 byte unsigned integer
short :: G.Get Word16
short =
  G.getWord16be

-- | a 2 byte two's complement integer
signedShort :: G.Get Int16
signedShort =
  G.getInt16be

-- | a short n, followed by n bytes representing an UTF-8 string
string :: G.Get T.Text
string =
  _shortLen >>= _string
  
-- | an int n, followed by n bytes representing an utf-8 string
longString :: G.Get T.Text
longString =
  _intLen >>= _string


newtype UUID = UUID B.ByteString

-- | a 16 bytes long uuid
uuid :: G.Get UUID
uuid = UUID <$> _blob 16

-- | a list of n elements
--
-- it is called list in the cassandra protocoll definitions
list :: G.Get a ->  G.Get (V.Vector a)
list g = do
  len <- _shortLen
  V.replicateM len g

-- | an int n, followed by n bytes if n >= 0
bytes :: G.Get (Maybe B.ByteString)
bytes =
  _intLen >>= _maybe _blob


data Value a
  = Value a
  | NullValue
  | NotSet

-- | an int n, followed by n bytes if n >= 0.  -1 null, -2 not set
value :: G.Get (Value B.ByteString)
value =
  _intLen >>= _value _blob

-- | a short n, followed by n bytes if n >= 0
--
-- XXX: duh? short is defined as unsigned?  I assume here it is signed
shortBytes :: G.Get (Maybe B.ByteString)
shortBytes =
  _signedShortLen >>= _maybe _blob

-- | a paird of <id><value> where id is a short
option :: G.Get (Word16, Value B.ByteString)
option =
  (,) <$> short <*> value

-- option list is just list option

-- | a short n followed by n key/value pairs. key is always string.
--
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
  pure $ Scientific.scientific unscaled (fromIntegral $ -scale)

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
inet =
  (,) <$> ipAddress <*> int

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

-- | a short giving the consistency
consistency :: G.Get Consistency
consistency =
  toEnum <$> _shortLen
  


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
-- protocol document is useless on this, see
-- https://github.com/datastax/java-driver/blob/57724bd63c3038e3728260c7640abcea06f33b86/driver-core/src/main/java/com/datastax/driver/core/VIntCoding.java
--
varint :: G.Get Integer
varint = 
  go 0 0

  where
    go !n !acc = do
      b <- G.getWord8
      case b of
        0xFF ->
          -- it's negative, eg 80 -> FF80
        x | x <= 0x7F -> pure $ acc `shift` (8*n)
        x -> go (n + 1) (acc `shift` (8*n))
        

