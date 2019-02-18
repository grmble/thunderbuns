module Data.ByteString.D64
  ( encodeBytes
  , decodeBytes
  , encodeBytes'
  , decodeBytes'
  , encodeWord64
  , decodeWord64
  , lpad
  , alphabet
  , invertabet
  ) where

import Control.Exception
import Control.Monad.ST (runST)
import Data.Bits ((.&.), (.|.), unsafeShiftL, unsafeShiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as L
import Data.Char (chr)
import Data.Semigroup ((<>))
import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Word (Word64, Word8)
import Text.Printf

alphabet :: B.ByteString
alphabet = ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"

invertabet :: VU.Vector Int
invertabet = invertAlphabet alphabet

invertAlphabet :: B.ByteString -> VU.Vector Int
invertAlphabet as =
  runST $ do
    vec <- VUM.replicate 256 (-1 :: Int)
    let n = B.length as
        loop i
          | i == n = return ()
        loop i = do
          let w8 = as `B.index` i
          let i' = fromIntegral w8 :: Int
          VUM.write vec i' i
          loop $ i + 1
    loop 0
    VU.freeze vec

encodeDigit :: GHC.Word.Word8 -> Char
encodeDigit w8 = chr $ fromIntegral $ alphabet `B.index` fromIntegral w8

decodeDigit :: GHC.Word.Word8 -> Int
decodeDigit w8 =
  let x = invertabet VU.! fromIntegral w8
   in if x < 0
        then throw $ UnknownCode w8
        else x

data CodecError
  = UnknownCode Word8
  | EmptyInput
  deriving (Typeable)

instance Show CodecError where
  show EmptyInput = "empty input"
  show (UnknownCode w8) = printf "unknown d64 code: %#02x" w8

instance Exception CodecError

encodeBytes :: L.ByteString -> L.ByteString
encodeBytes bs =
  BB.toLazyByteString
    (if (srcLen `rem` 3) > 0
       then result <> BB.char7 (encodeDigit finalHang)
       else result)
  where
    (result, srcLen, finalHang) =
      L.foldl'
        (\(b, pos, hang) w8 ->
           case pos `rem` 3 of
             0 -> (b <> BB.char7 enc, pos + 1, hang')
               where enc = encodeDigit (w8 `unsafeShiftR` 2)
                     hang' = (w8 .&. 0x3) `unsafeShiftL` 4
             1 -> (b <> BB.char7 enc, pos + 1, hang')
               where enc = encodeDigit (hang .|. (w8 `unsafeShiftR` 4))
                     hang' = (w8 .&. 0xF) `unsafeShiftL` 2
             2 -> (b <> BB.char7 enc1 <> BB.char7 enc2, pos + 1, hang')
               where enc1 = encodeDigit (hang .|. (w8 `unsafeShiftR` 6))
                     enc2 = encodeDigit (w8 .&. 0x3F)
                     hang' = 0
             _ -> error "rem 3 should be 0, 1 or 2")
        (BB.byteString "", 0 :: Int, 0 :: Word8)
        bs

encodeBytes' :: B.ByteString -> B.ByteString
encodeBytes' = L.toStrict . encodeBytes . L.fromStrict

decodeBytes :: L.ByteString -> L.ByteString
decodeBytes bs = BB.toLazyByteString result
  where
    (result, _, _) =
      L.foldl'
        (\(b, pos, hang) w8 ->
           let !dec = decodeDigit w8
            in case pos `rem` 4 of
                 0 -> (b, pos + 1, hang')
                   where hang' = dec `unsafeShiftL` 2
                 1 ->
                   ( b <> BB.char8 (chr (hang .|. (dec `unsafeShiftR` 4)))
                   , pos + 1
                   , hang')
                   where hang' = dec `unsafeShiftL` 4
                 2 ->
                   ( b <> BB.char8 (chr (hang .|. (dec `unsafeShiftR` 2)))
                   , pos + 1
                   , hang')
                   where hang' = dec `unsafeShiftL` 6
                 3 -> (b <> BB.char8 (chr (hang .|. dec)), pos + 1, 0)
                 _ -> error "rem 4 should be 0, 1, 2, or 3")
        (BB.byteString "", 0 :: Int, 0 :: Int)
        bs

decodeBytes' :: B.ByteString -> B.ByteString
decodeBytes' = L.toStrict . decodeBytes . L.fromStrict

lpad :: Int -> L.ByteString -> L.ByteString
lpad n bs =
  let len = L.length bs
      delta = fromIntegral n - len
   in if delta > 0
        then L.append (L.replicate delta (B.head alphabet)) bs
        else bs

encodeWord64 :: Word64 -> L.ByteString
encodeWord64 0 = "."
encodeWord64 x = (L.reverse . BB.toLazyByteString) $ loop (BB.byteString "") x
  where
    loop :: BB.Builder -> Word64 -> BB.Builder
    loop b 0 = b
    loop b n =
      let !q = n `unsafeShiftR` 6
          !r = fromIntegral (n .&. 0x3F) :: Int
       in loop (b <> BB.char7 (chr (fromIntegral (alphabet `B.index` r)))) q

decodeWord64 :: L.ByteString -> Word64
decodeWord64 bs =
  case L.uncons bs of
    Nothing -> throw EmptyInput
    Just (h, t) -> loop ((fromIntegral . decodeDigit) h) t
  where
    loop :: Word64 -> L.ByteString -> Word64
    loop !acc rest =
      case L.uncons rest of
        Nothing -> acc
        Just (h, t) ->
          let !dec = fromIntegral (decodeDigit h) :: Word64
              !acc' = (acc `unsafeShiftL` 6) .|. dec
           in loop acc' t
