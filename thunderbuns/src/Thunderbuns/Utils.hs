module Thunderbuns.Utils
  ( fromJustOrError
  , supervise
  , microSeconds
  ) where

import Control.Concurrent (threadDelay)
import System.Log.Bunyan.Context (someException)
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO
import System.Random (getStdRandom, randomR)
import Thunderbuns.Tlude
import UnliftIO.Exception (SomeException, catch, finally, throwString)

-- | Supervise some important action.
--
-- If there is a (sync) exception, the action will be entered again
-- after a random delay between minmu and maxmu (= microseonds).
--
-- The name is used as a name for the logger.
-- Note that a new logger is created in the loop,
-- this is so the log level can be changed.
supervise ::
     forall r m. (Bunyan r m, MonadUnliftIO m)
  => Text
  -> Int
  -> Int
  -> m ()
  -> m ()
supervise name minmu maxmu action =
  forever go `finally` logInfo ("Terminating " <> name)
  where
    go =
      withNamedLogger name id $ do
        logAndRun `catch` ignoreException
        r <- liftIO $ getStdRandom (randomR (minmu, maxmu))
        logDebug ("delaying for " <> toText (show r) <> " μm after exception")
        liftIO $ threadDelay r
    -- log and ignore the exception
    ignoreException :: SomeException -> m ()
    ignoreException ex =
      logRecord ERROR (someException ex) "caught exception in supervise"
    --
    -- log and run the action
    logAndRun = logInfo ("Supervising " <> name) *> action

-- Convert seconds to microseconds for 'supervise'
microSeconds :: Int -> Int
microSeconds s = s * 1000000

-- | Just the result, or throw a descriptive error
fromJustOrError :: MonadIO m => String -> Maybe a -> m a
fromJustOrError _ (Just x) = pure x
fromJustOrError msg Nothing = throwString msg
