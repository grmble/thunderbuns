{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Free
  ( module Data.Time.Clock
  , module Data.Time.Clock.System
  , module Data.Time.Clock.POSIX
  , FreeClockF(..)
  , HasSystemTime(..)
  , getCurrentTime
  , getSystemTime
  , getPOSIXTime
  , freeClockIO
  , freeClockPure
  ) where

import Control.Lens.PicoLens
import Control.Monad.Free.Church
import Control.Monad.Free.TH
import Control.Monad.Reader
import Data.Time.Clock hiding (getCurrentTime)
import qualified Data.Time.Clock as C
import Data.Time.Clock.POSIX hiding (getCurrentTime, getPOSIXTime)
import qualified Data.Time.Clock.POSIX as C
import Data.Time.Clock.System hiding (getSystemTime)
import qualified Data.Time.Clock.System as C
import Text.Show.Functions ()

-- | FreeClockF describes our clock operations
data FreeClockF x
  = GetCurrentTime (UTCTime -> x) -- ^ get the current time
  | GetSystemTime (SystemTime -> x) -- ^ get the current system time - supposed to be faster than UTCTime
  | GetPOSIXTime (POSIXTime -> x) -- ^ get the time in posix format
  deriving (Show, Functor)

$(makeFree ''FreeClockF)

-- | Interpret the free clock in the IO Monad
-- |
-- | Runs the IO actions from Data.Time.Clock
freeClockIO :: MonadIO m => FreeClockF a -> m a
freeClockIO (GetCurrentTime f) = f <$> liftIO C.getCurrentTime
freeClockIO (GetSystemTime f) = f <$> liftIO C.getSystemTime
freeClockIO (GetPOSIXTime f) = f <$> liftIO C.getPOSIXTime

-- | Reader env for pure freeclocks.
-- |
-- | SystemTime is actually the primitive used by POSIX and UTC Time
class HasSystemTime a where
  currentTime :: Lens' a SystemTime

instance HasSystemTime SystemTime where
  currentTime = id

-- | Interpret the freeclock from a Reader
freeClockPure :: (HasSystemTime r, MonadReader r m) => FreeClockF a -> m a
freeClockPure = go
  where
    askTime f = asks (f . view currentTime)
    go (GetCurrentTime f) = askTime (f . systemToUTCTime)
    go (GetSystemTime f) = askTime f
    go (GetPOSIXTime f) = askTime (f . systemToPOSIXTime)
