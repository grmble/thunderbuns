module Varint.GetSpec where

import Data.Int
import Data.Serialize.Get
import Database.CQL4.Varint.Get
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "numberOfExtrabytesToRead" $ do
    it "can handle bytes with MSB unset" $
      numberOfExtraBytesToRead 0 `shouldBe` 0
    it "can handle bytes with MSB unset (upper bound)" $
      numberOfExtraBytesToRead 0x7F `shouldBe` 0
    it "can handle single extra byte" $
      numberOfExtraBytesToRead 0x80 `shouldBe` 1
    it "can handle two extra bytes" $ numberOfExtraBytesToRead 0xc0 `shouldBe` 2
  describe "firstByteValueMask" $ do
    it "can handle 0 extra bytes" $ firstByteValueMask 0 `shouldBe` 0xFF
    it "can hancle single extra byte" $ firstByteValueMask 0x1 `shouldBe` 0x7F
    it "can hancle two extra bytes" $ firstByteValueMask 0x2 `shouldBe` 0x3F
    it "can hancle 8 extra bytes" $ firstByteValueMask 0x8 `shouldBe` 0x00
  describe "unsignedVarint" $ do
    it "can handle 0" $ runGet unsignedVarint "\x00" `shouldBe` Right 0
    it "can handle MSB unset (upper bound)" $
      runGet unsignedVarint "\x7f" `shouldBe` Right 0x7f
    it "can handle single extra byte" $
      runGet unsignedVarint "\x81\x00" `shouldBe` Right 0x100
    it "can handle single extra byte (upper bound)" $
      runGet unsignedVarint "\xBF\xFF" `shouldBe` Right 0x3fff
    it "can handle two extra bytes" $
      runGet unsignedVarint "\xC1\x00\x00" `shouldBe` Right 0x10000
    it "can handle two extra bytes (upper bound)" $
      runGet unsignedVarint "\xDF\xFF\xFF" `shouldBe` Right 0x1fffff
  describe "zigZag64" $
    it "decode . encode == id" $
    property $ \x -> decodeZigZag64 (encodeZigZag64 x) == (x :: Int64)
  describe "varint" $ do
    it "can decode 0" $ runGet varint "\x00" `shouldBe` Right 0
    it "can decode -1" $ runGet varint "\x01" `shouldBe` Right (-1)
    it "can decode 1" $ runGet varint "\x02" `shouldBe` Right 1
    it "can decode 512" $ runGet varint "\x84\x00" `shouldBe` Right 512
    it "can decode -512" $ runGet varint "\x83\xff" `shouldBe` Right (-512)
