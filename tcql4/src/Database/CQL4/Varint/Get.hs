module Database.CQL4.Varint.Get where

import Data.Int
import Data.Bits
import qualified Data.Serialize.Get as G
import Data.Word

varint :: G.Get Int64
varint = decodeZigZag64 <$> unsignedVarint

--
-- transcription of VIntCoding from datastax java driver
--

unsignedVarint :: G.Get Word64
unsignedVarint = do
  b <- G.getWord8
  let n = numberOfExtraBytesToRead b
  go n (fromIntegral $ b .&. firstByteValueMask n)

  where
    go 0 !acc =
      pure acc
    go n !acc = do
      b <- G.getWord8
      go (n - 1) (fromIntegral b `xor` (acc `shift` 8))


-- count number of upper bits - just invert all of the bits
-- only called with negative numbers, so when upcast all new upper bits are also set
numberOfExtraBytesToRead :: Word8 -> Int
numberOfExtraBytesToRead b =
  if b < 0x80
     then 0
     else countLeadingZeros (complement b)

-- and this with the first byte to give the value part
firstByteValueMask :: Int -> Word8
firstByteValueMask n =
  0xFF `shiftR` n

decodeZigZag64 :: Word64 -> Int64
decodeZigZag64 w =
  fromIntegral $ (w `shiftR` 1) `xor` negate (w .&. 1)

encodeZigZag64 :: Int64 -> Word64
encodeZigZag64 i =
  fromIntegral $ (i `shiftL` 1) `xor` (i `shiftR` 63)
