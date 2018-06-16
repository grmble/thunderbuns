module Thunderbuns.ValidateSpec where

import Data.Either
import qualified Data.Text as T
import Test.Hspec
import Thunderbuns.Validate

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "default validators" $ do
    it "int" $ unV <$> validate 1 `shouldBe` Right (1 :: Int)
    it "integer" $ unV <$> validate 17 `shouldBe` Right (17 :: Int)
    it "double" $
      unV <$> validate 3.1415297 `shouldBe` Right (3.1415297 :: Double)
    it "bool" $ unV <$> validate True `shouldBe` Right True
  describe "enum validation" $
    it "is always right" $
    unV <$> validate' Nothing enumValidator Foo `shouldBe` Right Foo
  describe "symbol validation" $ do
    it "valid" $
      unV <$>
      validate' Nothing symbolValidator "A17.xx_x" `shouldBe` Right "A17.xx_x"
    it "must be non-empty" $
      unV <$> validate' Nothing symbolValidator "" `shouldSatisfy` isLeft
    it "must start with a letter" $
      unV <$> validate' Nothing symbolValidator "0A" `shouldSatisfy` isLeft
    it "are not stripped " $
      unV <$> validate' Nothing symbolValidator " A " `shouldSatisfy` isLeft
  describe "text validation" $ do
    it "valid" $
      unV <$>
      validate ("  hello. i'm X. " :: T.Text) `shouldBe` Right "hello. i'm X."
    it "must not contain semicolons" $
      unV <$> validate ("  hello; i'm X. " :: T.Text) `shouldSatisfy` isLeft
    it "must be non-empty" $
      unV <$> validate ("" :: T.Text) `shouldSatisfy` isLeft
    it "must not contain newlines" $
      unV <$> validate ("line1\nline2" :: T.Text) `shouldSatisfy` isLeft
  describe "user record validation" $ do
    it "valid" $
      unV <$>
      validate (User "D'Artagnan" Foo 25) `shouldBe`
      (Right $ User "D'Artagnan" Foo 25)
    it "invalid age" $
      unV <$> validate (User "D'Artagnan" Foo 250) `shouldSatisfy` isLeft

data MyEnum
  = Foo
  | Bar
  | Baz
  deriving (Show, Eq, Enum)

data User = User
  { name :: T.Text
  , status :: MyEnum
  , age :: Int
  } deriving (Show, Eq)

instance DefaultValidator User where
  defaultValidator _ u =
    User <$> defaultValidator (Just "name") (name u) <*>
    enumValidator (Just "status") (status u) <*>
    inRange 0 200 (Just "age") (age u)
