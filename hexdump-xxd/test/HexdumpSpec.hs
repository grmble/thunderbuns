module HexdumpSpec where

import Data.ByteString.Hexdump
import Test.Hspec

-- not needed, but nice for ghci
main :: IO ()
main = hspec spec


spec :: Spec
spec =

  describe "hexdump" $

    it "same result as xxd" $
      hexdump "12345\n"
        `shouldBe`
        "00000000: 3132 3334 350a                           12345.\n"
