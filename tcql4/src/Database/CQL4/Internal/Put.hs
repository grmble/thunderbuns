module Database.CQL4.Internal.Put where

import Data.Serialize.Put as P
import Database.CQL4.Types
import Data.Int
import Data.Bits
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Data.Foldable

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
    foldFlags f acc =
      (1 `shiftL` fromEnum f) .|. acc

-- | a short n followed by n key-value pairs. key is always string
map :: (a -> P.Put) -> M.HashMap T.Text a -> P.Put
map vf hm = do
  let s = M.size hm
  _putShortLen s
  for_ (M.toList hm) $ \(k,v) -> do
    string k
    vf v

-- | a short n, followed by n bytes representing an UTF-8 string
string :: T.Text -> P.Put
string = _withShortLen . _utf8
  
  
_putShortLen :: Int -> P.Put
_putShortLen = P.putWord16be . fromIntegral

_withShortLen :: B.ByteString -> P.Put
_withShortLen bs = do
  _putShortLen $ B.length bs
  P.putByteString bs
  
-- | utf8 encode the text
_utf8 :: T.Text -> B.ByteString
_utf8 = TE.encodeUtf8

  

