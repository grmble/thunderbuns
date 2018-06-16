module Thunderbuns.Server.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant
import Servant.Server (Handler)
import Thunderbuns.Exception
import Thunderbuns.Logging

instance HasLogger r => MonadTLogger (ReaderT r Handler) where
  localLogger n ctx action = unwrapRIO $ localLogger n ctx (WrappedRIO action)
  logRecord pri obj msg = unwrapRIO $ logRecord pri obj msg
  getSystemTime = unwrapRIO getSystemTime

servantErr :: ServantErr -> T.Text -> ServantErr
servantErr err msg = err {errBody = BL.fromStrict $ TE.encodeUtf8 msg}

authenticationFailed ::
     (MonadTLogger m, MonadError ServantErr m) => T.Text -> m a
authenticationFailed logMsg = do
  let msg = "Authentication failed - wrong username or password"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err403 msg

authorizationDenied ::
     (MonadTLogger m, MonadError ServantErr m) => T.Text -> m a
authorizationDenied logMsg = do
  let msg = "Authorization denied - token invalid or expired"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err403 msg

internalError :: (MonadTLogger m, MonadError ServantErr m) => T.Text -> m a
internalError logMsg = do
  let msg = "Internal error"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err500 msg

mapError :: ReaderT r (ExceptT ThunderbunsException IO) a -> ReaderT r Handler a
mapError ma =
  ReaderT $ \r -> do
    ea <- liftIO $ runExceptT $ runReaderT ma r
    liftEither $ first tbServantErr ea

tbServantErr :: ThunderbunsException -> ServantErr
tbServantErr (AuthorizationDenied _) =
  servantErr err403 "Authorization denied - token invalid or expired"
tbServantErr (AuthenticationFailed _) =
  servantErr err403 "Authentication failed - wrong username or password"
tbServantErr (InternalError _ _) = servantErr err500 "Internal error"
tbServantErr (ValidationException ms _) =
  servantErr err400 ("Validation error: " <> T.pack (L.intercalate ", " ms))
