{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Irc.Config where

import Control.Lens.TH (makeClassy)
import qualified Data.Text.Encoding as T
import Dhall (Interpret)
import Thunderbuns.Tlude

-- ! Irc Server Configuration
data ServerConfig = ServerConfig
  { host :: !Text
  , port :: !Natural
  , tls :: !Bool
  , serverPassword :: !(Maybe Text)
  , nick :: !Text
  , fullname :: !Text
  , nicksrvPassword :: !(Maybe Text)
  , channels :: ![Text]
  } deriving (Generic, Eq, Show)

$(makeClassy ''ServerConfig)

instance Interpret ServerConfig

connectionSettings :: ServerConfig -> (Int, ByteString)
connectionSettings srv =
  let h = T.encodeUtf8 (host srv)
   in (fromIntegral $ port srv, h)
