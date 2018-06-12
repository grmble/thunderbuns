module FreeTimeSpec where

import Control.Lens.PicoLens
import Control.Monad.Free.Church
import Data.Int
import Data.Time.Free
import Data.Word
import Test.Hspec
import Test.QuickCheck

newtype MyEnv = MyEnv
  { myEnvTime :: SystemTime
  } deriving (Show)

myEnvTimeL :: Lens' MyEnv SystemTime
myEnvTimeL k a = fmap (\t -> a {myEnvTime = t}) (k (myEnvTime a))

instance HasSystemTime MyEnv where
  currentTime = myEnvTimeL

-- not needed, but nice for ghci
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "freeclock io" $
    it "io actions can be run" $ do
      _ <- foldF freeClockIO getCurrentTime
      _ <- foldF freeClockIO getSystemTime
      _ <- foldF freeClockIO getPOSIXTime
      (1 :: Int) + 1 `shouldBe` 2
  describe "freeclock pure" $ do
    it "systemtime can be set" $
      property $ \(s :: Int64, n :: Word32) -> do
        let sys = MkSystemTime s n
        let sys' = foldF freeClockPure getSystemTime sys
        sys' `shouldBe` sys
    it "posixtime is correctly converted" $
      property $ \(s :: Int64, n :: Word32) -> do
        let sys = MkSystemTime s n
        let pos = foldF freeClockPure getPOSIXTime sys
        pos `shouldBe` systemToPOSIXTime sys
    it "utctime is correctly converted" $
      property $ \(s :: Int64, n :: Word32) -> do
        let sys = MkSystemTime s n
        let pos = foldF freeClockPure getCurrentTime sys
        pos `shouldBe` systemToUTCTime sys
