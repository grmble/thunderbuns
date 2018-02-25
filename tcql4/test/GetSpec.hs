module GetSpec where

import qualified Data.Serialize.Get as G
import Database.CQL4.Get
import Database.CQL4.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "frameHeader" $ do
    it "decode request header" $
      G.runGet frameHeader "\x04\0\0\x41\0\x4e\xad\xbe\xef" `shouldBe`
      Right
        FrameHeader
          { frameVersion = RequestFrame
          , frameFlags = []
          , frameStream = 0x41
          , frameOpCode = OpError
          , frameLength = 0x4eadbeef
          }
    it "decode response header" $
      G.runGet frameHeader "\x84\0\0\x41\0\x4e\xad\xbe\xef" `shouldBe`
      Right
        FrameHeader
          { frameVersion = ResponseFrame
          , frameFlags = []
          , frameStream = 0x41
          , frameOpCode = OpError
          , frameLength = 0x4eadbeef
          }
    it "decode flags" $
      G.runGet frameHeader "\x04\x09\0\x41\0\x4e\xad\xbe\xef" `shouldBe`
      Right
        FrameHeader
          { frameVersion = RequestFrame
          , frameFlags = [FlagCompress, FlagWarning]
          , frameStream = 0x41
          , frameOpCode = OpError
          , frameLength = 0x4eadbeef
          }
    it "decode stream id / length" $
      G.runGet frameHeader "\x84\0\x3e\xef\0\x00\x00\x00\x41" `shouldBe`
      Right
        FrameHeader
          { frameVersion = ResponseFrame
          , frameFlags = []
          , frameStream = 0x3eef
          , frameOpCode = OpError
          , frameLength = 0x41
          }
    it "decode op code" $
      G.runGet frameHeader "\x04\0\0\x41\x10\x4e\xad\xbe\xef" `shouldBe`
      Right
        FrameHeader
          { frameVersion = RequestFrame
          , frameFlags = []
          , frameStream = 0x41
          , frameOpCode = OpAuthSuccess
          , frameLength = 0x4eadbeef
          }
