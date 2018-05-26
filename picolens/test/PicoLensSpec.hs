module PicoLensSpec where

import Control.Lens.PicoLens
import Test.Hspec
import Test.QuickCheck

-- not needed, but nice for ghci
main :: IO ()
main = hspec spec

data SomeRecord = SomeRecord
  { anInt :: Int
  , aString :: String
  } deriving (Show, Eq)

recordInt :: Lens' SomeRecord Int
recordInt k a = fmap (\b -> a {anInt = b}) (k (anInt a))

recordString :: Lens' SomeRecord String
recordString k a = fmap (\b -> a {aString = b}) (k (aString a))

spec :: Spec
spec =
  describe "view" $ do
    it "view gets the contents" $
      property $ \(i :: Int, s :: String) -> do
        let rec = SomeRecord {anInt = i, aString = s}
        view recordInt rec `shouldBe` i
        view recordString rec `shouldBe` s
    it "over will set field1" $
      property $ \(i :: Int, d :: Int) -> do
        let rec = SomeRecord i ""
        over recordInt (+ d) rec `shouldBe` rec {anInt = i + d}
    it "over will set field2" $
      property $ \(s :: String, d :: String) -> do
        let rec = SomeRecord 0 s
        over recordString (++ d) rec `shouldBe` rec {aString = s ++ d}
