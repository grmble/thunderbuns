module Thunderbuns.Auth where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Error (eitherCryptoError)
import Crypto.KDF.Argon2
import qualified Crypto.Random as Random
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.CQL4
import Jose.Jwa (JwsAlg(..))
import Jose.Jws (hmacDecode, hmacEncode)
import Jose.Jwt (IntDate(..), unJwt)
import Thunderbuns.Auth.Types
import Thunderbuns.Config
import Thunderbuns.Config.DB
import Thunderbuns.Config.Jwt
import Thunderbuns.DB.Internal (runDB)
import Thunderbuns.Exception
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
     ( HasDbConfig r
     , MonadReader r m
     , MonadError ThunderbunsException m
     , MonadAuthDb m
     )
  => V UserPass
  -> m ()
addUser up = do
  o <- asks (view (dbConfig . passwdOptions))
  let p = TE.encodeUtf8 (pass (unV up))
  -- 16 bytes or 128 bits are sufficient salt/hash lengths according to docs
  s <- getRandomBytes 16
  case eitherCryptoError $ hash (argonOpts o) p s 16 of
    Left err -> throwError $ internalError (show err)
    Right h -> upsertPasswd (user (unV up)) o s h

authenticate ::
     ( HasJwtConfig r
     , MonadAuthDb m
     , MonadReader r m
     , MonadError ThunderbunsException m
     , MonadTLogger m
     )
  => V UserPass
  -> m Token
authenticate up = do
  let p = TE.encodeUtf8 (pass (unV up))
  (o, s, h) <- selectPasswd (user (unV up))
  case eitherCryptoError $ hash (argonOpts o) p s 16 of
    Left err -> throwError $ internalError ("hash: " ++ show err)
    Right h' ->
      if h == h'
        then getPOSIXTime >>= newJwtToken (user (unV up))
        else logError "password did not match stored hash" *>
             throwError authenticationFailed

argonOpts :: PasswdOptions -> Options
argonOpts (Argon2Options i m p) =
  Options (fromIntegral i) (fromIntegral m) (fromIntegral p) Argon2id Version13

-- | Generate a new JWT Token for the user
--
-- It will expirate at current time + configured lifetime
newJwtToken ::
     ( HasJwtConfig r
     , MonadReader r m
     , MonadError ThunderbunsException m
     , MonadTLogger m
     )
  => Username
  -> POSIXTime
  -> m Token
newJwtToken u t = do
  s <- decodeJwtSecret
  claims <- newClaims u t
  case hmacEncode HS512 s (BL.toStrict $ A.encode claims) of
    Left jwtErr -> do
      logError $ T.pack $ show jwtErr
      throwError $ internalError ("hmacEncode.error: " ++ show jwtErr)
    Right tk -> pure $ Token (TE.decodeUtf8 $ unJwt tk)

decodeJwtSecret ::
     (HasJwtConfig r, MonadReader r m, MonadError ThunderbunsException m)
  => m B.ByteString
decodeJwtSecret = do
  t16 <- asks (view (jwtConfig . secret))
  let (s, err) = B16.decode $ TE.encodeUtf8 t16
  if err == ""
    then pure s
    else throwError $ internalError ("B16.decode: " ++ show err)

decodeToken' ::
     B.ByteString -> POSIXTime -> B.ByteString -> Either T.Text Claims
decodeToken' tk t s = do
  (_, bs) <- first (T.pack . show) $ hmacDecode s tk
  c <- first (T.pack . show) $ A.eitherDecodeStrict' bs
  if jwtExp c < IntDate t
    then Left "token expired"
    else Right c

decodeToken ::
     ( HasJwtConfig r
     , MonadReader r m
     , MonadTLogger m
     , MonadError ThunderbunsException m
     )
  => B.ByteString
  -> m Claims
decodeToken tk = do
  t <- getPOSIXTime
  s <- decodeJwtSecret
  -- a changed secret will cause the hmacDecode to fail (message: bad signature)
  (_, bs) <- liftEither $ first (const authorizationDenied) $ hmacDecode s tk
  c <-
    liftEither $
    first (\e -> internalError ("json decode: " ++ e)) $
    A.eitherDecodeStrict' bs
  if jwtExp c < IntDate t
    then logError "token expired" *> throwError authorizationDenied
    else pure c

newClaims ::
     (HasJwtConfig r, MonadReader r m) => Username -> POSIXTime -> m Claims
newClaims u t = do
  dt <- asks (view (jwtConfig . lifetime))
  let ex = t + realToFrac dt
  pure Claims {jwtSub = u, jwtExp = IntDate ex}

instance HasDbConnection r =>
         MonadAuthDb (ReaderT r (ExceptT ThunderbunsException IO)) where
  upsertPasswd n o s h =
    runDB $
    execute
      Quorum
      "insert into tb.passwd (username, config, salt, hash) values (?, ?, ?, ?)"
      [ TextValue n
      , BlobValue (BL.toStrict $ A.encode o)
      , BlobValue s
      , BlobValue h
      ]
  selectPasswd n =
    runDB $ do
      let cql = "select config, salt, hash from tb.passwd where username=?"
      rows <- executeQuery Quorum cql [TextValue n]
      (mo, s, h) <-
        extractSingleRow ((,,) <$> extract <*> extract <*> extract) rows
      case A.decode (BL.fromStrict mo) of
        Nothing -> throwError $ messageException "error argon2 opts"
        Just o -> pure (o, s, h)
  getRandomBytes n = liftIO $ Random.getRandomBytes n
