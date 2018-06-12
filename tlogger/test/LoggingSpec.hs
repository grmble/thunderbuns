module LoggingSpec where

import Control.Lens.PicoLens
import Control.Monad.Free.Church
import Control.Monad.Free.Combinators
import Control.Monad.Reader
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as M
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
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
      runReaderT standardLogAction rl
      records <- readIORef var
      length records `shouldBe` 2
    it "rootlogger debug" $ do
      var <- newIORef []
      rl <- rootLogger "root" DEBUG (handler var)
      runReaderT standardLogAction rl
      records <- readIORef var
      length records `shouldBe` 4
    it "rootlogger error" $ do
      var <- newIORef []
      rl <- rootLogger "root" ERROR (handler var)
      runReaderT standardLogAction rl
      records <- readIORef var
      length records `shouldBe` 0
    it "rootlogger info/childLogger debug" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      atomically $ modifyTVar (view priorityMapL rl) (M.insert "child" DEBUG)
      runReaderT standardLogAction rl
      records <- readIORef var
      length records `shouldBe` 3
  describe "check free implementation" $
    it "works like the io version" $ do
      var <- newIORef []
      rl <- rootLogger "root" INFO (handler var)
      runReaderT (foldF treeLoggingIO standardFreeAction) rl
      records <- readIORef var
      length records `shouldBe` 2
      head records `shouldSatisfy` (\(LogRecord x) -> M.lookup "x" x == Just "17")
      head (tail records) `shouldSatisfy` (\(LogRecord x) -> isNothing $ M.lookup "x" x)
  describe "check duration helper" $
    it "should compute suitable headers" $ do
      (ctx, msg) <- duration <$> SC.getSystemTime <*> SC.getSystemTime
      show msg `shouldContain` "completed in"
      ctx `shouldSatisfy` (isJust . M.lookup "duration")
  where
    handler var logrec = modifyIORef var (logrec :)

standardLogAction :: HasLogger r => ReaderT r IO ()
standardLogAction = do
  logInfo "info@root"
  logDebug "debug@root"
  localLogger "child" (M.singleton "x" "17") $ do
    logInfo "info@child"
    logDebug "debug@child"

standardFreeAction :: F (TreeF (T.Text, AT.Object) LoggingF) ()
standardFreeAction = do
  logInfo "info@free root"
  logDebug "debug@free root"
  localLogger "child" (M.singleton "x" "17") $ do
    logInfo "info@free child"
    logDebug "debug@free child"
