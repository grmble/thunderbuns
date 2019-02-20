module Thunderbuns.Irc.Config where

import Data.Text (Text)
import Dhall (Generic, Interpret)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

data Server = Server
  { host :: !Text
  , port :: !Integer
  , ssl :: !Bool
  , serverPassword :: !Text
  , nick :: !Text
  , nicksrvPassword :: !Text
  } deriving (Generic, Eq, Show)

instance Interpret Server


connectionSettings :: Server -> (Int, ByteString)
connectionSettings srv =
  let h = T.encodeUtf8 (host srv)
  in  (fromIntegral $ port srv, h)
