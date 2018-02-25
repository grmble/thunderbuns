module Internal.PutSpec where

import qualified Data.Serialize.Put as P
import Database.CQL4.Internal.Put
import Database.CQL4.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "frameHeader" $ do
    it "encode request header" $
      P.runPut (frameHeader RequestFrame [] 0x41 OpError 0x4eadbeef) `shouldBe`
      "\x04\0\0\x41\0\x4e\xad\xbe\xef"
    it "encode response header" $
      P.runPut (frameHeader ResponseFrame [] 0x41 OpError 0x4eadbeef) `shouldBe`
      "\x84\0\0\x41\0\x4e\xad\xbe\xef"
    it "encode flags" $
      P.runPut
        (frameHeader
           RequestFrame
           [FlagCompress, FlagWarning]
           0x41
           OpError
           0x4eadbeef) `shouldBe`
      "\x04\x09\0\x41\0\x4e\xad\xbe\xef"
    it "encode stream id/length" $
      P.runPut (frameHeader RequestFrame [] 0x3eef OpError 0x41) `shouldBe`
      "\x04\0\x3e\xef\0\0\0\0\x41"
    it "encode op code error" $
      P.runPut (frameHeader RequestFrame [] 0x41 OpAuthSuccess 0x4eadbeef) `shouldBe`
      "\x04\0\0\x41\x10\x4e\xad\xbe\xef"
    it "encode op code startup" $
      P.runPut (frameHeader RequestFrame [] 0x41 OpStartup 0x4eadbeef) `shouldBe`
      "\x04\0\0\x41\x1\x4e\xad\xbe\xef"
