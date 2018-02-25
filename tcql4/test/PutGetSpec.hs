module PutGetSpec where

import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import qualified Database.CQL4.Get as CG
import qualified Database.CQL4.Put as CP
import Data.Text
import Data.String
import Database.CQL4.Types
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary Text where
  arbitrary = fromString <$> (arbitrary :: Gen String)

spec :: Spec
spec = do
  describe "check put/get identity" $ do
    it "string" $ property $
      \txt -> G.runGet CG.string (P.runPut (CP.string txt)) == Right (txt::Text)
