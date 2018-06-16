{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Thunderbuns.Server.Auth where

import Control.Monad.Reader
import Crypto.Random (getRandomBytes)
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Jose.Jwt
import Network.HTTP.Types (hAuthorization)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth
import Thunderbuns.Auth
import Thunderbuns.Auth.Types
import Thunderbuns.Config
import Thunderbuns.Config.Jwt
import Thunderbuns.Logging
import Thunderbuns.Server.Types
import Thunderbuns.Validate

type UserAPI
  -- authorize
   = ReqBody '[ JSON] UserPass :> Post '[ JSON] Token

userAPI :: Proxy UserAPI
userAPI = Proxy

userServerT ::
     (HasLogger r, HasDbConnection r, HasJwtConfig r)
  => ServerT UserAPI (ReaderT r Handler)
userServerT = authenticateUser
  where
    authenticateUser up = mapError (validateM up >>= authenticate)

-- | Generate a random secret suitable for jwt tokens
--
-- basically, 512 random bits, base16 encoded
randomSecret :: IO T.Text
randomSecret = do
  bs <- Crypto.Random.getRandomBytes (512 `div` 8)
  pure $ TE.decodeUtf8 $ B16.encode bs

-- | JWT Auth handler, as needed by servant
jwtAuthHandler ::
     (HasJwtConfig e, HasLogger e) => e -> AuthHandler Request JwtClaims
jwtAuthHandler e = mkAuthHandler $ \req -> runReaderT (jwtAuthHandler' req) e

-- | JWT Auth Handler in our ReaderT e Handler Monad
jwtAuthHandler' ::
     (HasJwtConfig e, HasLogger e) => Request -> ReaderT e Handler JwtClaims
jwtAuthHandler' req = do
  auth <-
    maybe
      (authorizationDenied "can not find authorization header")
      pure
      (L.lookup hAuthorization (requestHeaders req))
  logDebug ("Authorization header: " <> TE.decodeUtf8 auth)
  tk <-
    case Atto.parseOnly bearer auth of
      Left err -> authorizationDenied $ T.pack err
      Right x -> pure x
  logDebug ("JWT from header: " <> TE.decodeUtf8 tk)
  mapError $ decodeToken tk
  where
    bearer :: Atto.Parser B.ByteString
    bearer = do
      _ <- Atto.string "Bearer "
      Atto.skipWhile (0x20 ==)
      Atto.takeByteString
