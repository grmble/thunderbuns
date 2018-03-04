{-# LANGUAGE CPP #-}

-- | CQL Exceptions
module Database.CQL4.Exceptions where

import Data.Monoid ((<>))
import Database.CQL4.Types
import GHC.Stack (callStack, prettySrcLoc)
import GHC.Stack.Types (CallStack, HasCallStack, getCallStack)
import UnliftIO.Exception

-- | CQL Exception
--
-- This will be thrown on errors in the CQL Code.
-- Note there may be also GetExceptions from Cereal
data CQLException
  = CQLException String
                 CallStack
  | MessageException String
                     Message
                     CallStack
  | FromValueException String
                       TypedValue
                       CallStack

instance Show CQLException where
  show = showCQLException

instance Exception CQLException

showCQLException :: CQLException -> String
showCQLException ex =
  case ex of
    CQLException str cs -> showEx "CQLException" str cs
    MessageException str msg cs ->
      showEx "MessageException" (str <> ": " <> (show msg)) cs
    FromValueException str tv cs ->
      showEx "FromValueException" (str <> ": " <> (show tv)) cs
  where
    showEx n msg cs =
      n <> ": " <> msg <> "\n\nStack:\n" <> concatMap go (getCallStack cs)
    go (x, y) = "  " <> x <> ", (" <> prettySrcLoc y <> ")\n"

-- | Smart constructor that deals with the call stack
cqlException :: HasCallStack => String -> CQLException
cqlException str = CQLException str callStack

-- | Smart constructor that deals with the call stack
messageException :: HasCallStack => String -> Message -> CQLException
messageException str msg = MessageException str msg callStack

-- | Smart constructor that deals with the call stack
fromValueException :: HasCallStack => String -> TypedValue -> CQLException
fromValueException str typ = FromValueException str typ callStack
