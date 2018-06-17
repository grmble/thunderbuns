{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.AuthSpec where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.TH (makeClassy)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock.System as SC
import Jose.Jwt
import Test.Hspec
import Thunderbuns.Auth
import Thunderbuns.Auth.Types
import Thunderbuns.Config.DB
import Thunderbuns.Config.Jwt
import Thunderbuns.Exception
import Thunderbuns.Logging
import Thunderbuns.Server.Types
import UnliftIO.Exception

data TestEnv = TestEnv
  { _db :: DbConfig
  , _jwt :: JwtConfig
  , _constantTime :: SystemTime
  , _constantBytes :: B.ByteString
  }

$(makeClassy ''TestEnv)

instance HasDbConfig TestEnv where
  dbConfig = db

instance HasJwtConfig TestEnv where
  jwtConfig = jwt

instance HasSystemTime TestEnv where
  systemTime = constantTime

type TestM
   = WrappedReader (ReaderT TestEnv (StateT (Maybe (PasswdOptions, Salt, Hash)) (Except ThunderbunsException)))

instance MonadAuthDb TestM where
  upsertPasswd _ o s h = put (Just (o, s, h))
  selectPasswd _ = fromJust <$> get
  getRandomBytes _ = asks (view constantBytes)

runTestM ::
     TestEnv
  -> Maybe (PasswdOptions, Salt, Hash)
  -> TestM a
  -> Either ThunderbunsException (a, Maybe (PasswdOptions, Salt, Hash))
runTestM r s m =
  m & (unwrapReader >>> flip runReaderT r >>> flip runStateT s >>> runExcept)

runTestM' ::
     TestEnv
  -> Maybe (PasswdOptions, Salt, Hash)
  -> TestM a
  -> (a, Maybe (PasswdOptions, Salt, Hash))
runTestM' r s m =
  case runTestM r s m of
    Left err -> impureThrow $ stringException (show err)
    Right x -> x

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let env =
        TestEnv
          { _db =
              DbConfig
                { _host = "host"
                , _port = 1337
                , _passwdOptions =
                    Argon2Options
                      {iterations = 1, memory = 132000, parallelism = 1}
                }
          , _jwt = JwtConfig {_secret = "1f04", _lifetime = 3600}
          , _constantTime = SC.MkSystemTime 0 0
          , _constantBytes =
              B.pack (concat (replicate 4 [0xde, 0xad, 0xbe, 0xef]))
          }
  describe "testing authentication with known salts and passwords" $ do
    let (_, s) =
          runTestM'
            env
            Nothing
            (validateTB (UserPass "user" "pass") >>= addUser)
    it
      "addUser preserves current passwdOptions and uses getRandomBytes for salt" $ do
      fmap (view _1) s `shouldBe` Just (view (dbConfig . passwdOptions) env)
      fmap (view _2) s `shouldBe` Just (view constantBytes env)
    it "authenticate will accept the same password" $ do
      let (x, _) =
            runTestM'
              env
              s
              (validateTB (UserPass "user" "pass") >>= authenticate)
      -- no errors!
      liftIO $ print x
      x `shouldSatisfy` const True
    it "authenticate will fail with a different password" $ do
      let x =
            runTestM
              env
              s
              (validateTB (UserPass "user" "WRONG") >>= authenticate)
      liftIO $ print x
      x `shouldSatisfy` isAuthenticationFailed
    it "authenticate will fail with a different salt" $ do
      let x =
            runTestM
              env
              (fmap (over _2 (<> "\xff")) s)
              (validateTB (UserPass "user" "pass") >>= authenticate)
      liftIO $ print x
      x `shouldSatisfy` isAuthenticationFailed
  describe "testing authorization with known salts and passwords" $ do
    let (tk, _) =
          runTestM' env Nothing $ do
            validateTB (UserPass "user" "pass") >>= addUser
            validateTB (UserPass "user" "pass") >>= authenticate
    it "authorization succeeds with token from authenticate" $ do
      let bs = T.encodeUtf8 (token tk)
      let (c, _) =
            runTestM'
              (over constantTime (const (SC.MkSystemTime 3600 0)) env)
              Nothing $
            decodeToken bs
      print c
      jwtSub c `shouldBe` Just "user"
    it "authorization fails after token expires" $ do
      let bs = T.encodeUtf8 (token tk)
      let x =
            runTestM
              (over constantTime (const (SC.MkSystemTime 3600 500000)) env)
              Nothing $
            decodeToken bs
      print x
      x `shouldSatisfy` isAuthorizationDenied
    it "authorization fails with changed secret" $ do
      let bs = T.encodeUtf8 (token tk)
      let x =
            runTestM
              (over (jwtConfig . secret) (const "ffffffff") env)
              Nothing $
            decodeToken bs
      print x
      x `shouldSatisfy` isAuthorizationDenied

isAuthorizationDenied :: Either ThunderbunsException a -> Bool
isAuthorizationDenied (Left (AuthorizationDenied _)) = True
isAuthorizationDenied _ = False

isAuthenticationFailed :: Either ThunderbunsException a -> Bool
isAuthenticationFailed (Left (AuthenticationFailed _)) = True
isAuthenticationFailed _ = False
