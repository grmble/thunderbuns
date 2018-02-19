{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Thunderbuns Logging

Logging for haskell, bunyan style.

https://github.com/trentm/node-bunyan

The output format is compatible with bunyan (command line tool!)


-}
module Thunderbuns.Logging
  ( Priority(..)
  , Logger
  , LogRecord
  , PureLogRecord
  , rootLogger
  , logger
  , logRecord
  , pureLogRecord
  , logM
  , errorM
  , warnM
  , infoM
  , debugM
  , consoleHandler
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict as M
import Data.Maybe (maybe)
import qualified Data.Text as T
import qualified Data.Time.Clock as C
import qualified Data.Time.Clock.System as SC
import Network.BSD (getHostName)
import System.Environment (lookupEnv)
import System.IO (stdout, hFlush)
import Text.Show.Functions

data Priority
  = FATAL
  | ERROR
  | WARN
  | INFO
  | DEBUG
  | TRACE
  deriving (Show, Eq)

intPriority :: Priority -> Int
intPriority FATAL = 60
intPriority ERROR = 50
intPriority WARN = 40
intPriority INFO = 30
intPriority DEBUG = 20
intPriority TRACE = 10

mkPriority :: Int -> Priority
mkPriority 60 = FATAL
mkPriority 50 = ERROR
mkPriority 40 = WARN
mkPriority 30 = INFO
mkPriority 20 = DEBUG
mkPriority _ = TRACE

data Logger = Logger
  { name :: {-# UNPACK #-}!T.Text
    -- ^ logger name
  , context :: !AT.Object
    -- ^ current context object - can be overwritten
  , handler :: !(LogRecord -> IO ())
    -- ^ handler for log records
  , rootContext :: !AT.Object
    -- ^ root context - contains everything that can be determined
    -- when the root logger is created.  Cant be overwritten.
  } deriving (Show)

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
rootLogger :: T.Text -> (LogRecord -> IO ()) -> IO Logger
rootLogger n h = do
  hn <- getHostName
  pid <- lookupEnv "$$"
  return
    Logger
      { name = n
      , context = M.empty
      , handler = h
      , rootContext =
          M.fromList
            [ ("v", A.Number 0)
            , ("hostname", A.String (T.pack hn))
            , ("pid", A.Number $ maybe 0 read pid)
            ]
      }

-- | Create a child logger with the given name and default properties
logger :: T.Text -> AT.Object -> Logger -> Logger
logger n ctx lg = lg {name = n, context = M.union ctx (context lg)}

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

{-- | A log record also has a time and everything from the root context.
--}
newtype LogRecord =
  LogRecord AT.Object
  deriving (Show, Eq)

logRecord :: PureLogRecord -> Logger -> IO LogRecord
logRecord (PureLogRecord obj) lg = do
  t <- SC.getSystemTime
  return $ LogRecord $ decorate t (context lg)
  where
    decorate t =
      M.union obj .
      M.insert "time" (A.toJSON $ SC.systemToUTCTime t) .
      M.union (rootContext lg)

logM :: Priority -> AT.Object -> T.Text -> Logger -> IO ()
logM pri obj msg lg = logRecord (pureLogRecord pri obj msg lg) lg >>= handler lg

infoM :: T.Text -> Logger -> IO ()
infoM = logM INFO M.empty

debugM :: T.Text -> Logger -> IO ()
debugM = logM DEBUG M.empty

errorM :: T.Text -> Logger -> IO ()
errorM = logM ERROR M.empty

warnM :: T.Text -> Logger -> IO ()
warnM = logM WARN M.empty
