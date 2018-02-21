module Thunderbuns.CQL4.Get where

import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Int (Int16, Int32, Int64)
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

bytesLen :: G.Get Int
bytesLen = fromIntegral <$> G.getInt32be

epochDate :: Day
epochDate = fromGregorian 1970 01 01

epochTimestamp :: UTCTime
epochTimestamp = UTCTime epochDate 0

-- | ascii - sequence of bytes 0 - 0x7F ascii :: G.Get T.Text
ascii :: G.Get T.Text
ascii =
  TE.decodeLatin1 <$> blob

-- | an eight-byte two's complement integer
bigint :: G.Get Int64
bigint =
  G.getInt64be

-- | a single byte, 0 denotes false, everything else is true
boolean :: G.Get Bool
boolean = do
  b <- G.getWord8
  pure $ b == 0

-- | blob - a blob of bytes
blob :: G.Get B.ByteString
blob =
  bytesLen >>= G.getBytes

-- | an unsigned integer representing days with epoch centered at 2^31
date :: G.Get Day
date = do
  d <- G.getInt32be
  pure $ addDays (fromIntegral d) epochDate

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

-- a 4-byte or 16-byte sequence denoting an IP4 or IPv6 address
inet :: G.Get IPAddress
inet = do
  len <- bytesLen
  case len of
    4 -> IPAddressV4 <$> G.getWord32be
    16 ->
      IPAddressV6 <$>
      ((,,,) <$> G.getWord32be <*> G.getWord32be <*> G.getWord32be <*>
       G.getWord32be)
    _ -> fail "IP address must be 4 or 16 bytes"

-- | a 4-byte two's complement integer
int :: G.Get Int32
int = G.getInt32be

-- | a list of n elements
--
-- it is called list in the cassandra protocoll definitions
list :: G.Get (V.Vector B.ByteString)
list = do
  len <- bytesLen
  V.replicateM len blob

-- | a map of n key value pairs
map :: G.Get (M.HashMap B.ByteString B.ByteString)
map = do
  len <- bytesLen
  M.fromList <$> (replicateM len ((,) <$> blob <*> blob))

-- | a set of n elements
set :: G.Get (S.HashSet B.ByteString)
set = do
  len <- bytesLen
  S.fromList <$> (replicateM len blob)

-- | a 2 byte two's complement integer
smallint :: G.Get Int16
smallint =
  G.getInt16be

-- | a utf-8 encoded string
text :: G.Get T.Text
text =
  blob >>= \bs ->
  case TE.decodeUtf8' bs of
    Left ex -> fail $ show ex
    Right txt -> pure txt

-- | an 8 byte two's complement long representing nanoseconds since midnight
time :: G.Get DiffTime
time = do
  ns <- G.getInt64be
  pure $ picosecondsToDiffTime (fromIntegral ns * 1000)

-- | an 8 byte two's complement integer representing milliseconds since epoch
timestamp :: G.Get UTCTime
timestamp = do
  ms <- G.getInt64be
  pure $ addUTCTime (realToFrac ms / 1000) epochTimestamp

-- XXX implement me
varint :: G.Get Integer
varint = undefined
