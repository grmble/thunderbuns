module Database.CQL4.Internal.Put where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import Data.Int
import qualified Data.List as L
import qualified Data.Scientific as Scientific
import Data.Serialize.IEEE754 (putFloat32be, putFloat64be)
import Data.Serialize.Put as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock
import Data.UUID as U
import Data.Word
import qualified Database.CQL4.Internal.Get as CG
import Database.CQL4.Internal.Types
import Database.CQL4.Types

frameHeader :: FrameVersion -> [FrameFlag] -> StreamID -> OpCode -> Int -> P.Put
frameHeader v fs i c len = do
  case v of
    RequestFrame -> P.putWord8 0x4
    ResponseFrame -> P.putWord8 0x84
  P.putWord8 $ foldr foldFlags 0 fs
  P.putInt16be i
  P.putWord8 $ fromIntegral $ fromEnum c
  P.putInt32be $ fromIntegral len
  where
    foldFlags f acc = (1 `shiftL` fromEnum f) .|. acc

-- | a short n followed by n key-value pairs. key is always string
map :: (a -> P.Put) -> M.HashMap T.Text a -> P.Put
map vf hm = do
  let s = M.size hm
  _putShortLen s
  for_ (M.toList hm) $ \(k, v) -> do
    string k
    vf v

-- | a short n, followed by n bytes representing an UTF-8 string
string :: T.Text -> P.Put
string = _withShortLen . _utf8

-- | an int n, followed by n bytes representing an UTF-8 string
longString :: T.Text -> P.Put
longString = _withIntLen . _utf8

-- | a 32 bit integer
int :: Int32 -> P.Put
int = P.putInt32be

-- | an int n, followed by n bytes
bytes :: B.ByteString -> P.Put
bytes = _withIntLen

-- | varint VALUE - i.e. it has a leading int length
varint :: Integer -> P.Put
varint i = do
  let bs = computeVarint i
  _putIntLen $ L.length bs
  for_ bs P.putWord8

decimal :: Scientific.Scientific -> P.Put
decimal i = do
  let unscaled = Scientific.coefficient i
  let scale = Scientific.base10Exponent i
  let bs = computeVarint unscaled
  _putIntLen (4 + L.length bs)
  int $ fromIntegral (-scale)
  for_ bs P.putWord8

computeVarint :: Integer -> [Word8]
computeVarint n =
  let nbs = natBytes' [] (abs n)
      bs = bytes' [] n (L.length nbs)
      mb = fromIntegral (head bs) :: Int8
  in case (n < 0, mb < 0) of
       (True, True) -> bs
       (True, False) -> 0xff : bs
       (False, True) -> 0 : bs
       (False, False) -> bs
  where
    natBytes' :: [Word8] -> Integer -> [Word8]
    natBytes' acc 0 =
      if L.null acc
        then [0]
        else acc
    natBytes' !acc m =
      natBytes' (fromIntegral (m .&. 0xFF) : acc) (m `shiftR` 8)
    bytes' acc _ 0 =
      if L.null acc
        then [0]
        else acc
    bytes' !acc !m cnt =
      bytes' (fromIntegral (m .&. 0xFF) : acc) (m `shiftR` 8) (cnt - 1)

-- | an 8 byte two's complement integer representing milliseconds since epoch
timestamp :: UTCTime -> P.Put
timestamp ts = do
  let dt = diffUTCTime ts CG._epochTimestamp
  P.putInt64be $ round $ dt * 1000

-- | an unsigned integer representing days with epoch centered at 2^31
date :: Day -> P.Put
date d = do
  let d' = diffDays d CG._epochDate
  P.putInt32be $ fromIntegral d'

-- | A consistency level
consistency :: Consistency -> P.Put
consistency cl = _putShortLen $ fromEnum cl

-- | a 4-byte or 16-byte sequence denoting an IP4 or IPv6 address
ipAddress :: IPAddress -> P.Put
ipAddress (IPAddressV4 w) = _putIntLen 4 *> P.putWord32be w
ipAddress (IPAddressV6 (w1, w2, w3, w4)) = do
  _putIntLen 16
  P.putWord32be w1
  P.putWord32be w2
  P.putWord32be w3
  P.putWord32be w4

queryFlags :: Query -> P.Put
queryFlags (Query _ _ vs sm psz pst sc dt) =
  P.putWord8
    ((if L.null vs
        then 0
        else 1) .|.
     (if sm
        then 2
        else 0) .|.
     maybe 0 (const 0x04) psz .|.
     maybe 0 (const 0x08) pst .|.
     maybe 0 (const 0x10) sc .|.
     maybe 0 (const 0x20) dt)

queryValues :: Query -> P.Put
queryValues (Query _ _ vs _ _ _ _ _) = do
  _putShortLen $ L.length vs
  for_ vs typedValue

typedValue :: TypedValue -> P.Put
-- XXX typedValue (CustomValue n v)
typedValue NullValue = _putIntLen 0
typedValue (TextValue str) = _withIntLen $ _utf8 str
typedValue (BlobValue bs) = _withIntLen bs
typedValue (BoolValue b) = do
  _putIntLen 1
  P.putInt8
    (if b
       then 1
       else 0)
typedValue (LongValue i) = _putIntLen 8 *> P.putInt64be i
typedValue (IntValue i) = _putIntLen 4 *> P.putInt32be i
typedValue (SmallintValue i) = _putIntLen 2 *> P.putInt16be i
typedValue (TinyintValue i) = _putIntLen 1 *> P.putInt8 i
typedValue (FloatValue f) = _putIntLen 4 *> putFloat32be f
typedValue (DoubleValue d) = _putIntLen 8 *> putFloat64be d
typedValue (DecimalValue i) = decimal i
typedValue (VarintValue vi) = varint vi
typedValue (UUIDValue uu) = _withIntLen $ LB.toStrict (U.toByteString uu)
typedValue (TimeUUIDValue uu) = _withIntLen $ LB.toStrict (U.toByteString uu)
typedValue (DateValue d) = _putIntLen 4 *> date d
typedValue (TimestampValue ts) = _putIntLen 8 *> timestamp ts
typedValue (InetValue ip) = ipAddress ip

_putShortLen :: Int -> P.Put
_putShortLen = P.putWord16be . fromIntegral

_putIntLen :: Int -> P.Put
_putIntLen = P.putInt32be . fromIntegral

_withShortLen :: B.ByteString -> P.Put
_withShortLen bs = do
  _putShortLen $ B.length bs
  P.putByteString bs

_withIntLen :: B.ByteString -> P.Put
_withIntLen bs = do
  _putIntLen $ B.length bs
  P.putByteString bs

-- | utf8 encode the text
_utf8 :: T.Text -> B.ByteString
_utf8 = TE.encodeUtf8
