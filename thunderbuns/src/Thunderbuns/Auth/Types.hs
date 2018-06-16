{-# LANGUAGE TemplateHaskell #-}
module Thunderbuns.Auth.Types where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString as B
import qualified Data.Text as T
import GHC.Generics (Generic)
import Thunderbuns.Validate (DefaultValidator(..), appmsg)

data UserPass = UserPass
  { user :: T.Text
  , pass :: T.Text
  } deriving (Generic, Eq, Show)

$(deriveJSON defaultOptions ''UserPass)

instance DefaultValidator UserPass where
  defaultValidator msg (UserPass u p) =
    UserPass <$> defaultValidator (appmsg msg "user") u <*>
    defaultValidator (appmsg msg "pass") p

newtype Token = Token
  { token :: T.Text
  } deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions ''Token)

type Salt = B.ByteString
type Hash = B.ByteString
type Username = T.Text

