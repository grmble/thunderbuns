module Internal.PutGetSpec where

import Data.Int
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Data.String
import Data.Text
import Data.Time.Calendar
import qualified Database.CQL4.Internal.Get as CG
import qualified Database.CQL4.Internal.Put as CP
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary Text where
  arbitrary = fromString <$> (arbitrary :: Gen String)

spec :: Spec
spec =
  describe "check put/get identity" $ do
    it "string" $
      property $ \txt ->
        G.runGet CG.string (P.runPut (CP.string txt)) == Right (txt :: Text)
    it "date" $
      property $ \(d :: Int32) ->
        let day = toInteger d
        in G.runGet CG.date (P.runPut (CP.date (ModifiedJulianDay day))) ==
           Right (ModifiedJulianDay day)
