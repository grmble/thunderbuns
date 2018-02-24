module Database.CQL4.Varint.Put where

import Control.Monad.ST
import Data.Bits
import Data.Int
import qualified Data.Serialize.Put as P
import Data.Foldable
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Unsafe
import Data.Word
import Database.CQL4.Varint.Get (encodeZigZag64, firstByteValueMask)

varint :: Int64 -> P.Put
varint i = do
  unsignedVarint $ encodeZigZag64 i

unsignedVarint :: Word64 -> P.Put
unsignedVarint w = do
  let n = computeUnsignedVIntSize w

  if n <= 1
    then P.putWord8 (fromIntegral w)
    else do
      let bs = revBytes n w
      for_ [0..(n - 1)] (\i -> P.putWord8 (bs ! i))

  where
    revBytes :: Int -> Word64 -> UArray Int Word8
    revBytes n v = runST $ do
      arr <- newArray_ (0, n - 1) :: ST s (STUArray s Int Word8)
      calcBytes arr (n - 1) v
      v0 <- readArray arr 0
      writeArray arr 0 (v0 .|. encodeExtraBytesToRead (n - 1))
      unsafeFreeze arr

    calcBytes arr i !v = do
      writeArray arr i (fromIntegral v)
      if i == 0
        then pure ()
        else calcBytes arr (i - 1) (v `shiftR` 8)
       



encodeExtraBytesToRead :: Int -> Word8
encodeExtraBytesToRead n =
  -- there is an extra bit in the value mask, so just invert
  complement (firstByteValueMask n)

-- compute number of bytes needed to encode unsigned varint
computeUnsignedVIntSize :: Word64 -> Int
computeUnsignedVIntSize w =
  -- with 1 to ensure magnitude <= 63
  let magnitude = countLeadingZeros (w .|. 1)
  in  fromIntegral $ (639 - magnitude*9) `shiftR` 6
