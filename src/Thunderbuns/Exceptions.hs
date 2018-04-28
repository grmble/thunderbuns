{-# LANGUAGE CPP #-}

module Thunderbuns.Exceptions where

import qualified Data.List as L
import Data.Monoid ((<>))
import GHC.Stack (callStack, prettySrcLoc)
import GHC.Stack.Types (CallStack, HasCallStack, getCallStack)
import UnliftIO.Exception

-- | Validation Exception
--
-- Will be thrown on validation errors
data ValidationException =
  ValidationException [String]
                      CallStack

instance Show ValidationException where
  show = showValidationException

instance Exception ValidationException

showValidationException :: ValidationException -> String
showValidationException (ValidationException ms cs) =
  showEx "ValidationExceptoin" (L.intercalate ", " ms)
  where
    showEx n msg =
      n <> ": " <> msg <> "\n\nStack:\n" <> concatMap go (getCallStack cs)
    go (x, y) = "  " <> x <> ", (" <> prettySrcLoc y <> ")\n"

-- | Smart constructor that deals with the call stack
validationException :: HasCallStack => [String] -> ValidationException
validationException ms = ValidationException ms callStack
