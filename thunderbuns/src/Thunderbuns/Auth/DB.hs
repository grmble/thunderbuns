module Thunderbuns.Auth.DB where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Error (eitherCryptoError)
import Crypto.KDF.Argon2
import qualified Crypto.Random as Random
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.CQL4
import Jose.Jwa (JwsAlg(..))
import Jose.Jws (hmacDecode, hmacEncode)
import Jose.Jwt
import Servant
import Thunderbuns.Auth.Types
import Thunderbuns.Config
import Thunderbuns.Config.DB
import Thunderbuns.Config.Jwt
import Thunderbuns.Logging
import Thunderbuns.Validate

-- | Auth DB Primitives
class Monad m =>
      MonadAuthDb m
  where
  upsertPasswd :: Username -> PasswdOptions -> Salt -> Hash -> m ()
  -- ^ insert or update the passwd table entry for the given username
  -- | select the passwd table entry for the given username
  selectPasswd :: Username -> m (PasswdOptions, Salt, Hash)
  -- | get n cryptographically secure random bytes
  getRandomBytes :: Int -> m B.ByteString

addUser ::
     (HasDbConfig r, MonadReader r m, MonadTLogger m, MonadError ServantErr m, MonadAuthDb m)
  => V UserPass
  -> m ()
addUser up = do
  o <- asks (view (dbConfig . passwdOptions))
  let p = TE.encodeUtf8 (pass (unV up))
  -- 16 bytes or 128 bits are sufficient salt/hash lengths according to docs
  s <- getRandomBytes 16
  case eitherCryptoError $ hash (argonOpts o) p s 16 of
    Left err -> internalError (T.pack $ show err)
    Right h -> upsertPasswd (user (unV up)) o s h

authenticate ::
     ( HasJwtConfig r
     , MonadAuthDb m
     , MonadReader r m
     , MonadError ServantErr m
     , MonadTLogger m
     )
  => V UserPass
  -> m Token
authenticate up = do
  let p = TE.encodeUtf8 (pass (unV up))
  (o, s, h) <- selectPasswd (user (unV up))
  case eitherCryptoError $ hash (argonOpts o) p s 16 of
    Left err -> internalError (T.pack (show err))
    Right h' -> if h == h'
      then getPOSIXTime >>= newJwtToken (user (unV up))
      else authenticationFailed "different hash with given password"

argonOpts :: PasswdOptions -> Options
argonOpts (Argon2Options i m p) =
  Options (fromIntegral i) (fromIntegral m) (fromIntegral p) Argon2id Version13

-- | Generate a new JWT Token for the user
--
-- It will expirate at current time + configured lifetime
newJwtToken ::
     (HasJwtConfig r, MonadReader r m, MonadError ServantErr m, MonadTLogger m)
  => Username
  -> POSIXTime
  -> m Token
newJwtToken u t = do
  s <- decodeJwtSecret
  claims <- newClaims u t
  case hmacEncode HS512 s (BL.toStrict $ A.encode claims) of
    Left jwtErr -> do
      logError $ T.pack $ show jwtErr
      throwError $ servantErr err500 "hmacEncode.error"
    Right tk -> pure $ Token (TE.decodeUtf8 $ unJwt tk)

decodeJwtSecret ::
     (HasJwtConfig r, MonadReader r m, MonadTLogger m, MonadError ServantErr m)
  => m B.ByteString
decodeJwtSecret = do
  t16 <- asks (view (jwtConfig . secret))
  let (s, err) = B16.decode $ TE.encodeUtf8 t16
  if err == ""
    then pure s
    else authorizationDenied (TE.decodeUtf8 err)

decodeToken' ::
     B.ByteString -> POSIXTime -> B.ByteString -> Either T.Text JwtClaims
decodeToken' tk t s = do
  (_, bs) <- lmap (T.pack . show) $ hmacDecode s tk
  c <- lmap (T.pack . show) $ A.eitherDecodeStrict' bs
  et <- note "no expiration in token" (jwtExp c)
  if et < IntDate t
    then Left "token expired"
    else Right c

decodeToken ::
     ( HasJwtConfig r
     , MonadReader r m
     , MonadTLogger m
     , MonadError ServantErr m
     )
  => B.ByteString
  -> m JwtClaims
decodeToken tk = do
  t <- getPOSIXTime
  s <- decodeJwtSecret
  logDebug ("decodeToken: s=" <> TE.decodeUtf8 s)
  case decodeToken' tk t s of
    Left err -> authorizationDenied err
    Right c -> pure c

emptyClaims :: JwtClaims
emptyClaims = JwtClaims Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newClaims ::
     (HasJwtConfig r, MonadReader r m) => Username -> POSIXTime -> m JwtClaims
newClaims u t = do
  dt <- asks (view (jwtConfig . lifetime))
  let ex = t + realToFrac dt
  pure $ emptyClaims {jwtSub = Just u, jwtExp = Just (IntDate ex)}

-- XXX types ?
servantErr :: ServantErr -> T.Text -> ServantErr
servantErr err msg = err {errBody = BL.fromStrict $ TE.encodeUtf8 msg}

-- XXX types ?
authenticationFailed ::
     (MonadTLogger m, MonadError ServantErr m) => T.Text -> m a
authenticationFailed logMsg = do
  let msg = "Authentication failed - wrong username or password"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err403 msg

-- XXX types ?
authorizationDenied ::
     (MonadTLogger m, MonadError ServantErr m) => T.Text -> m a
authorizationDenied logMsg = do
  let msg = "Authorization denied - token invalid or expired"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err403 msg

-- XXX types ?
internalError ::
  (MonadTLogger m, MonadError ServantErr m) => T.Text -> m a
internalError logMsg = do
  let msg = "Internal error"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err500 msg

-- XXX : bifunctor
-- XXX : util module
lmap :: (a -> b) -> Either a c -> Either b c
lmap f (Left e) = Left $ f e
lmap _ (Right r) = Right r

-- XXX : util module
note :: MonadError e m => e -> Maybe a -> m a
note e Nothing = throwError e
note _ (Just a) = pure a

-- XXX: util module
note' :: MonadError e m => m a -> Maybe a -> m a
note' e Nothing = e
note' _ (Just a) = pure a


instance HasDbConnection r => MonadAuthDb (ReaderT r Handler) where
  upsertPasswd n o s h =
    ask >>= dbConnection >>= liftIO . runConnection' go
    where
      go =
        execute
          Quorum
          "insert into tb_users.passwd (username, config, salt, hash) values (?, ?, ?, ?)"
          [ TextValue n
          , BlobValue (BL.toStrict $ A.encode o)
          , BlobValue s
          , BlobValue h
          ]
  selectPasswd n =
    ask >>= dbConnection >>= liftIO . runConnection' go
    where
      go = do
        let cql =
              "select config, salt, hash from tb_users.passwd where username=?"
        rows <- executeQuery Quorum cql [TextValue n]
        (mo, s, h) <-
          extractSingleRow ((,,) <$> extract <*> extract <*> extract) rows
        case A.decode (BL.fromStrict mo) of
          Nothing -> throwError $ messageException "error argon2 opts"
          Just o -> pure (o, s, h)
  getRandomBytes n = liftIO $ Random.getRandomBytes n
