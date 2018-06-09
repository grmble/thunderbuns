{-# LANGUAGE TemplateHaskell #-}

{- | Thunderbuns Logging -}
module Thunderbuns.Logging
  ( HasLogger(..)
  , Priority(..)
  , Logger
  , LogRecord
  , rootLogger
  , mkLogger
  , logger
  , loggerNamesL
  , priorityMapL
  , logRecord
  , logDebug
  , logInfo
  , logWarn
  , logError
  , duration
  , consoleHandler
  ) where

import Control.Lens.PicoLens (Lens', over, view)
import Control.Monad (unless, when)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Time.Clock.System as SC
import GHC.Generics
import Network.BSD (getHostName)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Show.Functions ()
import Thunderbuns.Process (getPid)
import UnliftIO.STM

data Priority
  = FATAL
  | ERROR
  | WARN
  | INFO
  | DEBUG
  | TRACE
  deriving (Generic, Show, Eq)

$(ATH.deriveJSON A.defaultOptions ''Priority)

intPriority :: Priority -> Int
intPriority FATAL = 60
intPriority ERROR = 50
intPriority WARN = 40
intPriority INFO = 30
intPriority DEBUG = 20
intPriority TRACE = 10

data Logger = Logger
  { name :: {-# UNPACK #-}!T.Text
    -- ^ logger name
  , context :: !AT.Object
    -- ^ current context object - can be overwritten
  , handler :: !(LogRecord -> IO ())
    -- ^ handler for log records
  , rootContext :: !AT.Object
    -- ^ root context - coKntains everything that can be determined
    -- when the root logger is created.  Cant be overwritten.
  , priority :: {-# UNPACK #-}!Int
    -- ^ log level of the logger
  , priorityMap :: {-# UNPACK #-}!(TVar (M.HashMap T.Text Priority))
    -- ^ map of logger names to priority
  , loggerNames :: {-# UNPACK #-}!(TVar (S.HashSet T.Text))
  }

instance Show Logger where
  show lg =
    "Logger name=" ++
    show (name lg) ++
    "\n    context=" ++
    show (context lg) ++
    "\n    rootContext=" ++
    show (rootContext lg) ++ "\n    priority=" ++ show (priority lg)

class HasLogger a where
  loggerL :: Lens' a Logger

instance HasLogger Logger where
  loggerL = id

loggerNamesL :: Lens' Logger (TVar (S.HashSet T.Text))
loggerNamesL k lg = fmap (\x -> lg {loggerNames = x}) (k (loggerNames lg))

priorityMapL :: Lens' Logger (TVar (M.HashMap T.Text Priority))
priorityMapL k lg = fmap (\x -> lg {priorityMap = x}) (k (priorityMap lg))

-- | Console handler function - prints to console
consoleHandler :: LogRecord -> IO ()
consoleHandler (LogRecord obj) = do
  LBSC8.putStrLn $ A.encode obj
  hFlush stdout

-- | Create a root logger object.
-- |
-- | There should only be one root logger.
-- | It contains the handling function, all
-- | other loggers should be children of the root
-- | logger.
rootLogger ::
     MonadIO m => T.Text -> Priority -> (LogRecord -> IO ()) -> m Logger
rootLogger n p h = do
  hn <- liftIO getHostName
  pid <- liftIO getPid
  priMap <- newTVarIO M.empty
  logSet <- newTVarIO S.empty
  return
    Logger
    { name = n
    , context = M.empty
    , handler = h
    , rootContext =
        M.fromList
          [ ("v", A.Number 0)
          , ("hostname", A.String (T.pack hn))
          , ("pid", A.Number $ fromIntegral pid)
          ]
    , priority = intPriority p
    , priorityMap = priMap
    , loggerNames = logSet
    }

-- | Create a child logger with the given name and default properties
--
-- At creation time, it will read shared config for the
-- loglevel of the given name.  The decision to log or not
-- is simply an integer comparison - so try to have long lived
-- child loggers.
mkLogger ::
     (HasLogger r, MonadIO m, MonadReader r m)
  => T.Text
  -> AT.Object
  -> m Logger
mkLogger n ctx = do
  lg <- asks (view loggerL)
  m <- readTVarIO (priorityMap lg)
  s <- readTVarIO (loggerNames lg)
  unless (S.member n s) $ atomically $ modifyTVar (loggerNames lg) (S.insert n)
  let pri = maybe (priority lg) intPriority $ M.lookup n m
  pure lg {name = n, context = M.union ctx (context lg), priority = pri}

-- | Locally use a child logger with the given name and context
logger ::
     (HasLogger r, MonadIO m, MonadReader r m)
  => T.Text
  -> AT.Object
  -> m a
  -> m a
logger n ctx action = do
  lg <- mkLogger n ctx
  local (over loggerL (const lg)) action

-- | A pure log record does not contain time yet
--
-- It's also still missing the root context.
--
-- It will have anything from the parent logger
-- plus `msg` and `level` (= priority)
newtype PureLogRecord =
  PureLogRecord AT.Object
  deriving (Show, Eq)

-- | A log record also has a time and everything from the root context.
newtype LogRecord =
  LogRecord AT.Object
  deriving (Show, Eq)

-- | Log a generic log record.
--
-- It takes a priority, context object and message.
logRecord ::
     (HasLogger r, MonadReader r m, MonadIO m)
  => Priority
  -> AT.Object
  -> T.Text
  -> m ()
logRecord pri obj msg = do
  lg <- asks (view loggerL)
  let pri' = intPriority pri
  when (pri' >= priority lg) $ do
    tm <- liftIO SC.getSystemTime
    liftIO $ handler lg (LogRecord (decorate lg tm (context lg)))
  where
    decorate lg t =
      M.insert "name" (A.String $ name lg) .
      M.insert "level" (A.Number $ fromIntegral $ intPriority pri) .
      M.insert "msg" (A.String msg) .
      M.union obj .
      M.insert "time" (A.toJSON $ SC.systemToUTCTime t) .
      M.union (rootContext lg)

-- | Helper to compute a duration.
--
-- This will compute a suitable context object and message
-- which can be logged using
--
--    x <- getSystemTime >>= duration
--    logRecord DEBUG (curry x)
duration :: MonadIO m => SC.SystemTime -> m (AT.Object, T.Text)
duration start = do
  end <- liftIO SC.getSystemTime
  let dur = double end - double start
  let ctx = M.singleton "duration" (AT.Number $ Scientific.fromFloatDigits dur)
  let msg = T.pack $ printf "completed in %dms" ((round $ 1000 * dur) :: Int)
  pure (ctx, msg)
  where
    double :: SC.SystemTime -> Double
    double sc =
      fromIntegral (SC.systemSeconds sc) +
      fromIntegral (SC.systemNanoseconds sc) / 1e9

logInfo :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logInfo = logRecord INFO M.empty

logDebug :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logDebug = logRecord DEBUG M.empty

logError :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logError = logRecord ERROR M.empty

logWarn :: (HasLogger r, MonadReader r m, MonadIO m) => T.Text -> m ()
logWarn = logRecord WARN M.empty
