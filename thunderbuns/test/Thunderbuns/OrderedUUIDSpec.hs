module Thunderbuns.OrderedUUIDSpec where

import Data.Maybe (fromJust)
import Data.UUID
import Data.UUID.Util
import Data.UUID.V1 (nextUUID)
import Data.Word (Word64)
import Thunderbuns.OrderedUUID
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

defaultUUID :: UUID
{-# NOINLINE defaultUUID #-}
defaultUUID = fromJust $ unsafePerformIO nextUUID

instance Arbitrary UUID where
  arbitrary = withTime defaultUUID <$> arbitrary

withTime :: UUID -> Word64 -> UUID
withTime uuid w64 = fromJust $ setTime (copy uuid) w64
  where
    copy u = fromJust (fromByteString (toByteString u))

spec :: Spec
spec =
  describe "Ordered UUID" $ do
    it "should be isomorphic to UUID" $
      property $ \u -> u == toUUID (orderedUUID u)
    it "should preserve order" $
      property $ \u1 u2 ->
        extractTime u1 `compare` extractTime u2 ==
        orderedUUID u1 `compare` orderedUUID u2
    it "timestamps should preserve order" $
      property $ \u1 u2 ->
        extractTime u1 `compare` extractTime u2 ==
        timestamp (orderedUUID u1) `compare` timestamp (orderedUUID u2)
    it "should preserve equality" $
      property $ \u1 u2 -> (u1 == u2) == (orderedUUID u1 == orderedUUID u2)
