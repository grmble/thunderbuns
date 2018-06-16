{-# LANGUAGE CPP #-}

module Thunderbuns.Exception
  ( ThunderbunsException(..)
  , validationException
  , authenticationFailed
  , authorizationDenied
  , internalError
  , maybeError
  ) where

import Control.Monad.Except (MonadError(..))
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
  | AuthorizationDenied CallStack
  | InternalError String
                  CallStack

instance Show ThunderbunsException where
  show = showTBE

instance Exception ThunderbunsException

showTBE :: ThunderbunsException -> String
showTBE (ValidationException ms cs) =
  showEx "ValidationException" (L.intercalate ", " ms) cs
showTBE (AuthenticationFailed cs) =
  showEx "AuthenticationFailed" "wrong username or password" cs
showTBE (AuthorizationDenied cs) =
  showEx "AuthorizationDenied" "invalid or expired token" cs
showTBE (InternalError s cs) = showEx "InternalError" s cs

showEx :: String -> String -> CallStack -> String
showEx n msg cs =
  n <> ": " <> msg <> "\n\nStack:\n" <> concatMap go (getCallStack cs)
  where
    go (x, y) = "  " <> x <> ", (" <> prettySrcLoc y <> ")\n"

-- | Smart constructor that deals with the call stack
validationException :: HasCallStack => [String] -> ThunderbunsException
validationException ms = ValidationException ms callStack

authenticationFailed :: HasCallStack => ThunderbunsException
authenticationFailed = AuthenticationFailed callStack

authorizationDenied :: HasCallStack => ThunderbunsException
authorizationDenied = AuthorizationDenied callStack

internalError :: HasCallStack => String -> ThunderbunsException
internalError s = InternalError s callStack

maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e Nothing = throwError e
maybeError _ (Just a) = pure a
