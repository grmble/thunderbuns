module Varint.PutSpec where

import Data.Serialize.Put
import Database.CQL4.Varint.Put
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unsignedVarint" $ do
    it "can handle 0" $ runPut (unsignedVarint 0) `shouldBe` "\x00"
    it "can handle MSB unset (upper bound)" $
      runPut (unsignedVarint 0x7f) `shouldBe` "\x7f"
    it "can handle single extra byte" $
      runPut (unsignedVarint 0x100) `shouldBe` "\x81\x00"
    it "can handle single extra byte (upper bound)" $
      runPut (unsignedVarint 0x3fff) `shouldBe` "\xBF\xFF"
    it "can handle two extra bytes" $
      runPut (unsignedVarint 0x10000) `shouldBe` "\xC1\x00\x00"
    it "can handle two extra bytes (upper bound)" $
      runPut (unsignedVarint 0x1fffff) `shouldBe` "\xDF\xFF\xFF"
