module Data.ByteString.Hexdump
  (hexdump)
where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldMap)
import Data.Int (Int32, Int64)
import qualified Data.List as L
import Data.Monoid ((<>))

-- | Produces a hexdump in the style of the unix xxd command
--
-- Takes a lazy bytestring and produces a lazy bytestring
hexdump :: LB.ByteString -> LB.ByteString
hexdump = BB.toLazyByteString . foldMap hexline . splitWithPos 16
  where
    hexline (pos, bs) = fmtPos pos <> fmtGroups bs <> fmtDisplay bs <> "\n"
    -- 0000001f: 
    fmtPos p = BB.int32HexFixed (fromIntegral p :: Int32) <> ": "
    -- 0102 0304 0506 ...
    fmtGroups bs =
      let gs = foldMap fmtGroup (splitAllAt 2 bs)
          glen = LB.length (BB.toLazyByteString gs)
          need = 40 - glen
       in gs <> BB.string7 (replicate (fromIntegral need) ' ') <> " "
    -- 0102 
    fmtGroup :: LB.ByteString -> BB.Builder
    fmtGroup g = BB.byteString (B16.encode (LB.toStrict g)) <> " "
    -- ...Hello...
    fmtDisplay :: LB.ByteString -> BB.Builder
    fmtDisplay bs = foldMap displayWord8 (LB.unpack bs)
    displayWord8 w8 =
      if w8 >= 0x20 && w8 <= 0x7F
        then BB.word8 w8
        else BB.byteString "."

splitAllAt :: Int64 -> LB.ByteString -> [LB.ByteString]
splitAllAt n = L.unfoldr go
  where
    go "" = Nothing
    go x = Just $ LB.splitAt n x

splitWithPos :: Int64 -> LB.ByteString -> [(Int64, LB.ByteString)]
splitWithPos n bs = L.unfoldr go (0, bs)
  where
    go (_, "") = Nothing
    go (p, x) =
      let (a, b) = LB.splitAt n x
       in Just ((p, a), (p + LB.length a, b))
