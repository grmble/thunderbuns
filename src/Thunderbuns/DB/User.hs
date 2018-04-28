{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.DB.User where

import Control.Lens (view)
import Control.Monad.Except
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
import Thunderbuns.Config
import Thunderbuns.Config.DB
import Thunderbuns.Validate

data UserPass = UserPass
  { user :: T.Text
  , pass :: T.Text
  } deriving (Show, Eq)

$(ATH.deriveJSON ATH.defaultOptions ''UserPass)

instance DefaultValidator UserPass where
  defaultValidator msg (UserPass u p) =
    UserPass <$> defaultValidator (appmsg msg "user") u <*>
    defaultValidator (appmsg msg "pass") p

addUser :: (HasDbConnection e, HasDbConfig e) => V UserPass -> ReaderT e IO ()
addUser up = do
  opts <- asks (view (dbConfig . passwdOptions))
  dbc <- ask >>= dbConnection
  liftIO $ runConnection' (addUser' opts up) dbc

addUser' :: PasswdOptions -> V UserPass -> ConnectionIO ()
addUser' o up = do
  let p = TE.encodeUtf8 (pass (unV up))
  -- 16 bytes or 128 bits are sufficient salt/hash lengths according to docs
  s <- liftIO (getRandomBytes 16)
  h <- liftIO (throwCryptoErrorIO $ hash (argonOpts o) p s 16)
  execute
    One
    "insert into tb_users.passwd (username, config, salt, hash) values (?, ?, ?, ?)"
    [ TextValue (user (unV up))
    , BlobValue (BL.toStrict $ encode o)
    , BlobValue s
    , BlobValue h
    ]

argonOpts :: PasswdOptions -> Options
argonOpts (Argon2Options i m p) =
  Options (fromIntegral i) (fromIntegral m) (fromIntegral p) Argon2id Version13

authorize :: HasDbConnection e => V UserPass -> ReaderT e IO Bool
authorize up = do
  dbc <- ask >>= dbConnection
  liftIO $ runConnection' (authorize' up) dbc

authorize' :: V UserPass -> ConnectionIO Bool
authorize' up = do
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
      pure $ h == h'
