{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.DB.Auth where

import Control.Lens (view)
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Crypto.Error (throwCryptoErrorIO)
import Crypto.KDF.Argon2
import Crypto.Random (getRandomBytes)
import Data.Aeson (decode, encode)
import qualified Data.Aeson.TH as ATH
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.CQL4
import GHC.Generics
import Thunderbuns.Config
import Thunderbuns.Config.DB
import Thunderbuns.Validate

data UserPass = UserPass
  { user :: T.Text
  , pass :: T.Text
  } deriving (Generic, Eq, Show)

$(ATH.deriveJSON ATH.defaultOptions ''UserPass)

instance DefaultValidator UserPass where
  defaultValidator msg (UserPass u p) =
    UserPass <$> defaultValidator (appmsg msg "user") u <*>
    defaultValidator (appmsg msg "pass") p

-- | Auth DB Primitives
class Monad m =>
      MonadAuthDB m
  where
  addUser :: V UserPass -> m ()
  -- ^ Add a user.  Does not do anything if the user already exists
  authenticate :: V UserPass -> m Bool
  -- ^ Authenticate a user.

data AuthDBF x
  = AddUserF (V UserPass)
             x
  | AuthenticateF (V UserPass)
                  (Bool -> x)
  deriving (Show, Functor)

instance MonadAuthDB (Free AuthDBF) where
  addUser up = liftF $ AddUserF up ()
  authenticate up = liftF $ AuthenticateF up id

interpretIO ::
     ( HasDbConnection r
     , HasDbConfig r
     , MonadReader r m
     , MonadIO m
     )
  => AuthDBF a
  -> m a
interpretIO (AddUserF up x) = do
  opts <- asks (view (dbConfig . passwdOptions))
  dbc <- ask >>= dbConnection
  liftIO $ runConnection' (addUser' opts up) dbc
  pure x
interpretIO (AuthenticateF up f) = do
  dbc <- ask >>= dbConnection
  f <$> liftIO (runConnection' (authenticate' up) dbc)


addUser' :: PasswdOptions -> V UserPass -> ConnectionIO ()
addUser' o up = do
  let p = TE.encodeUtf8 (pass (unV up))
  -- 16 bytes or 128 bits are sufficient salt/hash lengths according to docs
  s <- liftIO (getRandomBytes 16)
  h <- liftIO (throwCryptoErrorIO $ hash (argonOpts o) p s 16)
  execute
    One
    "insert into tb_users.passwd (username, config, salt, hash) values (?, ?, ?, ?) if not exists"
    [ TextValue (user (unV up))
    , BlobValue (BL.toStrict $ encode o)
    , BlobValue s
    , BlobValue h
    ]

argonOpts :: PasswdOptions -> Options
argonOpts (Argon2Options i m p) =
  Options (fromIntegral i) (fromIntegral m) (fromIntegral p) Argon2id Version13


authenticate' :: V UserPass -> ConnectionIO Bool
authenticate' up = do
  let p = TE.encodeUtf8 (pass (unV up))
  let cql = "select config, salt, hash from tb_users.passwd where username=?"
  rows <- executeQuery One cql [TextValue (user (unV up))]
  (mo, s, h) <-
    extractSingleRow
      ((,,) <$> extract <*> (extract :: ValueExtractorIO B.ByteString) <*>
       (extract :: ValueExtractorIO B.ByteString))
      rows
  case decode (BL.fromStrict mo) of
    Nothing -> throwError $ messageException "error argon2 opts"
    Just o -> do
      h' <- liftIO (throwCryptoErrorIO $ hash (argonOpts o) p s 16)
      pure (h == h')
