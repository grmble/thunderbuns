{-# LANGUAGE CPP #-}

-- | CQL Exceptions
--
-- It could be in Types.hs, but it needs CPP, so I rather have a short file.
module Database.CQL4.Exceptions where

import Data.Monoid ((<>))
import Database.CQL4.Internal.Types
import Database.CQL4.Types
import GHC.Stack (callStack, prettySrcLoc)
import GHC.Stack.Types (CallStack, HasCallStack, getCallStack)
import UnliftIO.Exception

-- | CQL Exception
--
-- This will be thrown on errors in the CQL Code.
-- Note there may be also GetExceptions from Cereal
data CQLException
  = CQLException ErrorMsgT
                 CallStack
  | MessageException String
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
    CQLException msg cs -> showEx "CQLException" (show msg) cs
    MessageException str cs -> showEx "MessageException" str cs
    FromValueException str tv cs ->
      showEx "FromValueException" (str <> ": " <> (show tv)) cs
  where
    showEx n msg cs =
      n <> ": " <> msg <> "\n\nStack:\n" <> concatMap go (getCallStack cs)
    go (x, y) = "  " <> x <> ", (" <> prettySrcLoc y <> ")\n"

-- | Smart constructor that deals with the call stack
cqlException :: HasCallStack => ErrorMsgT -> CQLException
cqlException err = CQLException err callStack

-- | Smart constructor that deals with the call stack
messageException :: HasCallStack => String -> CQLException
messageException str = MessageException str callStack

-- | Smart constructor that deals with the call stack
fromValueException :: HasCallStack => String -> TypedValue -> CQLException
fromValueException str typ = FromValueException str typ callStack
