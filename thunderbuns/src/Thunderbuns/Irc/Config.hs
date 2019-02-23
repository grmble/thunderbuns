module Thunderbuns.Irc.Config where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Dhall (Generic, Interpret)

-- ! Server Configuration
data Server = Server
  { host :: !Text
  , port :: !Integer
  , ssl :: !Bool
  , serverPassword :: !Text
  , nick :: !Text
  , fullname :: !Text
  , nicksrvPassword :: !Text
  } deriving (Generic, Eq, Show)

instance Interpret Server

connectionSettings :: Server -> (Int, ByteString)
connectionSettings srv =
  let h = T.encodeUtf8 (host srv)
   in (fromIntegral $ port srv, h)
