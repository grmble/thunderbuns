{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , app
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Thunderbuns.Logging

data User = User
  { userId :: Int
  , userFirstName :: String
  , userLastName :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[ JSON] [User]

startApp :: Int -> Logger -> IO ()
startApp port lg = do
  runReaderT (logDebug $ "Starting to serve on port " <> T.pack (show port)) lg
  run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
