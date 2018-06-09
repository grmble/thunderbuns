{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Server.User where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Generics
import Jose.Jwa (JwsAlg(..))
import Jose.Jws (hmacDecode, hmacEncode)
import Jose.Jwt
import Network.HTTP.Types (hAuthorization)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth
import Thunderbuns.Config
import Thunderbuns.Config.Jwt
import qualified Thunderbuns.DB.User as DBU
import Thunderbuns.Logging
import Thunderbuns.Validate

newtype Token = Token
  { token :: T.Text
  } deriving (Generic, Show, Eq)

$(ATH.deriveJSON ATH.defaultOptions ''Token)

type Username = T.Text

type UserAPI
  -- authorize
   = ReqBody '[ JSON] DBU.UserPass :> Post '[ JSON] Token

userAPI :: Proxy UserAPI
userAPI = Proxy

userServerT ::
     (HasLogger e, HasDbConnection e, HasJwtConfig e)
  => ServerT UserAPI (ReaderT e Handler)
userServerT = authenticateUser
  where
    authenticateUser up = validateM up >>= authenticate

authenticate ::
     (HasLogger e, HasDbConnection e, HasJwtConfig e)
  => V DBU.UserPass
  -> ReaderT e Handler Token
authenticate up = do
  isAuth <- DBU.authenticate up
  if isAuth
    then liftIO getPOSIXTime >>= newJwtToken (DBU.user (unV up))
    else authenticationFailed "password did not match stored hash"

servantErr :: ServantErr -> T.Text -> ServantErr
servantErr err msg = err {errBody = BL.fromStrict $ TE.encodeUtf8 msg}

authenticationFailed :: HasLogger e => T.Text -> ReaderT e Handler a
authenticationFailed logMsg = do
  let msg = "Authentication failed - wrong username or password"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err403 msg

authorizationDenied :: HasLogger e => T.Text -> ReaderT e Handler a
authorizationDenied logMsg = do
  let msg = "Authorization denied - token invalid or expired"
  logError (msg <> ": " <> logMsg)
  throwError $ servantErr err403 msg

-- | Generate a random secret suitable for jwt tokens
--
-- basically, 512 random bits, base16 encoded
randomSecret :: IO T.Text
randomSecret = do
  bs <- getRandomBytes (512 `div` 8)
  pure $ TE.decodeUtf8 $ B16.encode bs

-- | Generate a new JWT Token for the user
--
-- It will expirate at current time + configured lifetime
newJwtToken ::
     (HasJwtConfig e, HasLogger e)
  => Username
  -> POSIXTime
  -> ReaderT e Handler Token
newJwtToken u t = do
  s <- decodeJwtSecret
  claims <- newClaims u t
  case hmacEncode HS512 s (BL.toStrict $ A.encode claims) of
    Left jwtErr -> do
      logError $ T.pack $ show jwtErr
      throwError $ err500 {errBody = "Error encdoding JWT"}
    Right tk -> pure $ Token (TE.decodeUtf8 $ unJwt tk)

decodeJwtSecret ::
     (HasJwtConfig e, HasLogger e) => ReaderT e Handler B.ByteString
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
     (HasJwtConfig e, HasLogger e)
  => B.ByteString
  -> ReaderT e Handler JwtClaims
decodeToken tk = do
  t <- liftIO getPOSIXTime
  s <- decodeJwtSecret
  logDebug ("decodeToken: s=" <> TE.decodeUtf8 s)
  case decodeToken' tk t s of
    Left err -> authorizationDenied err
    Right c -> pure c

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

emptyClaims :: JwtClaims
emptyClaims = JwtClaims Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newClaims ::
     (HasJwtConfig e, MonadIO m)
  => Username
  -> POSIXTime
  -> ReaderT e m JwtClaims
newClaims u t = do
  dt <- asks (view (jwtConfig . lifetime))
  let ex = t + realToFrac dt
  pure $ emptyClaims {jwtSub = Just u, jwtExp = Just (IntDate ex)}

-- | JWT Auth handler, as needed by servant
jwtAuthHandler ::
     (HasJwtConfig e, HasLogger e) => e -> AuthHandler Request JwtClaims
jwtAuthHandler e = mkAuthHandler $ \req -> runReaderT (jwtAuthHandler' req) e

-- | JWT Auth Handler in our ReaderT e Handler Monad
jwtAuthHandler' ::
     (HasJwtConfig e, HasLogger e) => Request -> ReaderT e Handler JwtClaims
jwtAuthHandler' req = do
  auth <-
    note'
      (authorizationDenied "can not find authorization header")
      (L.lookup hAuthorization (requestHeaders req))
  logDebug ("Authorization header: " <> TE.decodeUtf8 auth)
  tk <-
    case Atto.parseOnly bearer auth of
      Left err -> authorizationDenied $ T.pack err
      Right x -> pure x
  logDebug ("JWT from header: " <> TE.decodeUtf8 tk)
  decodeToken tk
  where
    bearer :: Atto.Parser B.ByteString
    bearer = do
      _ <- Atto.string "Bearer "
      Atto.skipWhile (0x20 ==)
      Atto.takeByteString
