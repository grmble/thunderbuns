{-# LANGUAGE TemplateHaskell #-}
{- | Thunderbuns Logging

Logging for haskell, bunyan style.

https://github.com/trentm/node-bunyan

The output format is compatible with bunyan (command line tool!)


-}
module Thunderbuns.Logging
  ( HasLogger(..)
  , Priority(..)
  , Logger
  , LoggerIO
  , LogRecord
  , PureLogRecord
  , rootLogger
  , logger
  , loggerIO
  , loggerNamesL
  , priorityMapL
  , logRecord
  , logRecord'
  , pureLogRecord
  , logM
  , logIO
  , errorM
  , errorIO
  , warnM
  , warnIO
  , infoM
  , infoIO
  , debugM
  , debugIO
  , duration
  , consoleHandler
  ) where

import Control.Lens (Lens', view)
import Control.Monad (unless, when)
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Aeson.TH as ATH
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Time.Clock.System as SC
import Network.BSD (getHostName)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Show.Functions ()
import Thunderbuns.Process (getPid)
import UnliftIO.STM
import GHC.Generics

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
loggerNamesL k lg = fmap (\x -> lg { loggerNames = x }) (k (loggerNames lg))

priorityMapL :: Lens' Logger (TVar (M.HashMap T.Text Priority))
priorityMapL k lg = fmap (\x -> lg { priorityMap = x }) (k (priorityMap lg))

-- | LoggerIO is ReaderT HasLogger in the IO monad
--
-- Actually it's MonadIO so could be ExceptT x IO
type LoggerIO h m a
   = (MonadIO m, HasLogger h) =>
       ReaderT h m a

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
rootLogger :: T.Text -> Priority -> (LogRecord -> IO ()) -> IO Logger
rootLogger n p h = do
  hn <- getHostName
  pid <- getPid
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
logger :: (MonadIO m) => T.Text -> AT.Object -> Logger -> m Logger
logger n ctx lg = do
  m <- readTVarIO (priorityMap lg)
  s <- readTVarIO (loggerNames lg)
  unless (S.member n s) $ atomically $ modifyTVar (loggerNames lg) (S.insert n)
  let pri = maybe (priority lg) intPriority $ M.lookup n m
  pure lg {name = n, context = M.union ctx (context lg), priority = pri}

-- | Create a child logger in LoggerIO
loggerIO :: T.Text -> AT.Object -> LoggerIO h m Logger
loggerIO n ctx = do
  h <- ask
  let lg = view loggerL h
  liftIO $ logger n ctx lg

-- | A pure log record does not contain time yet
--
-- It's also still missing the root context.
--
-- It will have anything from the parent logger
-- plus `msg` and `level` (= priority)
newtype PureLogRecord =
  PureLogRecord AT.Object
  deriving (Show, Eq)

pureLogRecord :: Priority -> AT.Object -> T.Text -> Logger -> PureLogRecord
pureLogRecord pri obj msg lg = PureLogRecord $ decorate (context lg)
  where
    decorate =
      M.insert "name" (A.String $ name lg) .
      M.insert "level" (A.Number $ fromIntegral $ intPriority pri) .
      M.insert "msg" (A.String msg) . M.union obj

-- | A log record also has a time and everything from the root context.
newtype LogRecord =
  LogRecord AT.Object
  deriving (Show, Eq)

logRecord :: MonadIO m => PureLogRecord -> Logger -> m LogRecord
logRecord obj lg = do
  t <- liftIO SC.getSystemTime
  logRecord' t obj lg

logRecord' ::
     MonadIO m => SC.SystemTime -> PureLogRecord -> Logger -> m LogRecord
logRecord' tm (PureLogRecord obj) lg =
  pure $ LogRecord $ decorate tm (context lg)
  where
    decorate t =
      M.union obj .
      M.insert "time" (A.toJSON $ SC.systemToUTCTime t) .
      M.union (rootContext lg)

logM :: MonadIO m => Priority -> AT.Object -> T.Text -> Logger -> m ()
logM pri obj msg lg = do
  let pri' = intPriority pri
  when (pri' >= priority lg) $
    logRecord (pureLogRecord pri obj msg lg) lg >>= liftIO . handler lg

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

infoM :: T.Text -> Logger -> IO ()
infoM = logM INFO M.empty

debugM :: T.Text -> Logger -> IO ()
debugM = logM DEBUG M.empty

errorM :: T.Text -> Logger -> IO ()
errorM = logM ERROR M.empty

warnM :: T.Text -> Logger -> IO ()
warnM = logM WARN M.empty

logIO :: Priority -> AT.Object -> T.Text -> LoggerIO h m ()
logIO pri obj msg = do
  h <- ask
  liftIO $ logM pri obj msg (view loggerL h)

infoIO :: T.Text -> LoggerIO h m ()
infoIO = logIO INFO M.empty

debugIO :: T.Text -> LoggerIO h m ()
debugIO = logIO INFO M.empty

errorIO :: T.Text -> LoggerIO h m ()
errorIO = logIO INFO M.empty

warnIO :: T.Text -> LoggerIO h m ()
warnIO = logIO INFO M.empty
