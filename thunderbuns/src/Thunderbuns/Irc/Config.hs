{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Irc.Config where

import Control.Lens.TH (makeClassy)
import qualified Data.Text.Encoding as T
import Dhall (Interpret)
import Thunderbuns.Tlude

-- ! Server Configuration
data ServerConfig = ServerConfig
  { host :: !Text
  , port :: !Integer
  , tls :: !Bool
  , serverPassword :: !Text
  , nick :: !Text
  , fullname :: !Text
  , nicksrvPassword :: !Text
  , channels :: ![Text]
  } deriving (Generic, Eq, Show)

$(makeClassy ''ServerConfig)

instance Interpret ServerConfig

connectionSettings :: ServerConfig -> (Int, ByteString)
connectionSettings srv =
  let h = T.encodeUtf8 (host srv)
   in (fromIntegral $ port srv, h)
