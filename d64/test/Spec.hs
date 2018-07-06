import Data.ByteString.D64
import qualified Data.ByteString.Lazy as L
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, qcProps]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "encode empty string" $ "" @=? encodeBytes ""
    , testCase "encode string with null" $ ".." @=? encodeBytes "\x00"
    , testCase "encode 0x8d" $ "YF" @=? encodeBytes "\x8d"
    , testCase "decode empty string" $ "" @=? decodeBytes ""
    , testCase "decode string with null" $ "\x00" @=? decodeBytes ".."
    , testCase "decode 0x8d" $ "\x8d" @=? decodeBytes "YF"
    , testCase "encode word 0" $ "." @=? encodeWord64 0
    , testCase "encode word 1" $ "0" @=? encodeWord64 1
    , testCase "encode word 63" $ "z" @=? encodeWord64 63
    , testCase "encode word 64" $ "0." @=? encodeWord64 64
    , testCase "decode word 0" $ 0 @=? decodeWord64 "."
    , testCase "decode word 1" $ 1 @=? decodeWord64 "0"
    , testCase "decode word 63" $ 63 @=? decodeWord64 "z"
    , testCase "decode word 64" $ 64 @=? decodeWord64 "0."
    ]

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary

qcProps :: TestTree
qcProps =
  testGroup
    "(QuickCheck)"
    [ QC.testProperty "decodeBytes . encodeBytes == id" $ \bs ->
        bs == decodeBytes (encodeBytes bs)
    , QC.testProperty "decodeWord64 . encodeWord64 == id" $ \w64 ->
        w64 == decodeWord64 (encodeWord64 w64)
    ]
