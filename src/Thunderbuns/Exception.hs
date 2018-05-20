{-# LANGUAGE CPP #-}

module Thunderbuns.Exception
  ( ThunderbunsException(..)
  , validationException
  , authenticationDenied
  )
where

import qualified Data.List as L
import Data.Monoid ((<>))
import GHC.Stack (callStack, prettySrcLoc)
import GHC.Stack.Types (CallStack, HasCallStack, getCallStack)
import UnliftIO.Exception

-- | Thunderbuns Exception
--
-- Type collects all exceptions thrown by Thunderbuns
data ThunderbunsException
  = ValidationException [String]
                        CallStack
  -- ^ Input validation error
  | AuthenticationFailed CallStack -- ^ wrong username or password

instance Show ThunderbunsException where
  show = showTBE

instance Exception ThunderbunsException

showTBE :: ThunderbunsException -> String
showTBE (ValidationException ms cs) =
  showEx "ValidationException" (L.intercalate ", " ms) cs
showTBE (AuthenticationFailed cs) =
  showEx "AuthenticationFailed" "wrong username or password" cs

showEx :: String -> String -> CallStack -> String
showEx n msg cs =
  n <> ": " <> msg <> "\n\nStack:\n" <> concatMap go (getCallStack cs)
  where
    go (x, y) = "  " <> x <> ", (" <> prettySrcLoc y <> ")\n"

-- | Smart constructor that deals with the call stack
validationException :: HasCallStack => [String] -> ThunderbunsException
validationException ms = ValidationException ms callStack

authenticationDenied :: HasCallStack => ThunderbunsException
authenticationDenied = AuthenticationFailed callStack
