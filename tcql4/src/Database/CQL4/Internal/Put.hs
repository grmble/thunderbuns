module Database.CQL4.Internal.Put where

import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.HashMap.Strict as M
import Data.Int
import Data.Serialize.Put as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import qualified Database.CQL4.Internal.Get as CG
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

-- | an 8 byte two's complement integer representing milliseconds since epoch
timestamp :: UTCTime -> P.Put
timestamp ts = do
  let dt = diffUTCTime ts CG._epochTimestamp
  P.putInt64be $ round $ dt * 1000

consistency :: Consistency -> P.Put
consistency cl = _putShortLen $ fromEnum cl

queryFlags :: Query -> P.Put
queryFlags (UnboundQuery _ _ sm psz pst sc dt) =
  P.putWord8
    ((if sm
        then 2
        else 0) .|.
     maybe 0 (const 0x04) psz .|.
     maybe 0 (const 0x08) pst .|.
     maybe 0 (const 0x10) sc .|.
     maybe 0 (const 0x20) dt)

queryValues :: Query -> P.Put
queryValues UnboundQuery {} = pure ()

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
