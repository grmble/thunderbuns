module Internal.GetSpec where

import qualified Data.Serialize.Get as G
import Database.CQL4.Internal.Get
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
  describe "columnType" $ do
    it "custom" $
      G.runGet _columnType "\0\0\0\x01\x41" `shouldBe` Right (CTCustom "A")
    it "ascii" $ G.runGet _columnType "\x00\x01" `shouldBe` Right CTAscii
    it "bigint" $ G.runGet _columnType "\x00\x02" `shouldBe` Right CTBigint
    it "blob" $ G.runGet _columnType "\x00\x03" `shouldBe` Right CTBlob
    it "bool" $ G.runGet _columnType "\x00\x04" `shouldBe` Right CTBool
    it "counter" $ G.runGet _columnType "\x00\x05" `shouldBe` Right CTCounter
    it "decimal" $ G.runGet _columnType "\x00\x06" `shouldBe` Right CTDecimal
    it "double" $ G.runGet _columnType "\x00\x07" `shouldBe` Right CTDouble
    it "float" $ G.runGet _columnType "\x00\x08" `shouldBe` Right CTFloat
    it "int" $ G.runGet _columnType "\x00\x09" `shouldBe` Right CTInt
    it "timestamp" $
      G.runGet _columnType "\x00\x0b" `shouldBe` Right CTTimestamp
    it "uuid" $ G.runGet _columnType "\x00\x0c" `shouldBe` Right CTUuid
    it "varchar" $ G.runGet _columnType "\x00\x0d" `shouldBe` Right CTVarchar
    it "varint" $ G.runGet _columnType "\x00\x0e" `shouldBe` Right CTVarint
    it "timeuuid" $ G.runGet _columnType "\x00\x0f" `shouldBe` Right CTTimeuuid
    it "inet" $ G.runGet _columnType "\x00\x10" `shouldBe` Right CTInet
    it "date" $ G.runGet _columnType "\x00\x11" `shouldBe` Right CTDate
    it "time" $ G.runGet _columnType "\x00\x12" `shouldBe` Right CTTime
    it "smallint" $ G.runGet _columnType "\x00\x13" `shouldBe` Right CTSmallint
    it "tinyint" $ G.runGet _columnType "\x00\x14" `shouldBe` Right CTTinyint
    it "list" $
      G.runGet _columnType "\x00\x20\x00\x01" `shouldBe` Right (CTList CTAscii)
    it "map" $
      G.runGet _columnType "\x00\x21\x00\x01\x00\x02" `shouldBe`
      Right (CTMap CTAscii CTBigint)
    it "set" $
      G.runGet _columnType "\x00\x22\x00\x0d" `shouldBe` Right (CTSet CTVarchar)
    it "udt" $
      G.runGet
        _columnType
        "\x00\x30\x00\x01\x41\x00\x01\x42\x00\x01\x00\x01\x43\x00\x01" `shouldBe`
      Right (CTUDT "A" "B" [("C", CTAscii)])
    it "tuple" $
      G.runGet _columnType "\x00\x31\x00\x01\x00\x02" `shouldBe`
      Right (CTTuple [CTBigint])
