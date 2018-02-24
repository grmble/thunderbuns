module VarintSpec where

import Data.Int
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Data.Word
import qualified Database.CQL4.Varint.Get as GV
import qualified Database.CQL4.Varint.Put as PV
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unsignedVarint" $
    it "get (put) == id " $
    property $ \x ->
      G.runGet GV.unsignedVarint (P.runPut (PV.unsignedVarint x)) ==
      Right (x :: Word64)
  describe "varint" $
    it "get (put) == id " $
    property $ \x ->
      G.runGet GV.varint (P.runPut (PV.varint x)) == Right (x :: Int64)
