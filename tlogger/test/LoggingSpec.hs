module LoggingSpec where

import Control.Lens.PicoLens
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust)
import qualified Data.Time.Clock.System as SC
import Test.Hspec
import Thunderbuns.Logging
import UnliftIO.IORef
import UnliftIO.STM

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "check log levels" $ do
    it "rootlogger info" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 2
    it "rootlogger debug" $ do
      var <- newIORef []
      rl <- rootLogger "root" DEBUG (handler var)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 4
    it "rootlogger error" $ do
      var <- newIORef []
      rl <- rootLogger "root" ERROR (handler var)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 0
    it "rootlogger info/childLogger debug" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      atomically $ modifyTVar (view priorityMapL rl) (M.insert "child" DEBUG)
      _ <- runReaderT ioAction rl
      records <- readIORef var
      length records `shouldBe` 3
  describe "check duration helper" $
    it "should compute suitable headers" $ do
      (ctx, msg) <- duration <$> SC.getSystemTime <*> SC.getSystemTime
      show msg `shouldContain` "completed in"
      ctx `shouldSatisfy` (isJust . M.lookup "duration")
  describe "check pure (= non-logging version)" $
    it "should return fixed time" $ do
      let epoch :: SC.SystemTime = SC.MkSystemTime 0 0
      let x = runExcept $ runReaderT (unwrapReader testAction) epoch
      x `shouldBe` Right epoch
  where
    handler var logrec = modifyIORef var (logrec :)

standardAction :: MonadTLogger m => m SC.SystemTime
standardAction = do
  logInfo "info@root"
  logDebug "debug@root"
  localLogger "child" (M.singleton "x" "17") $ do
    logInfo "info@child"
    logDebug "debug@child"
    getSystemTime

ioAction :: HasLogger r => ReaderT r IO SC.SystemTime
ioAction = standardAction

testAction :: HasSystemTime r => WrappedReader (ReaderT r (Except String)) SC.SystemTime
testAction = standardAction

