module Thunderbuns.CmdLine where

import Conduit (ConduitM(..), (.|), mapC, runConduit, yield)
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CC
import Thunderbuns.Irc.Config (Server(..))
import System.Log.Bunyan (Logger(..), Priority(..), rootLogger, consoleHandler, logDebug)
import Dhall (input, auto)
import Data.Conduit.Network (runTCPClient, clientSettings, appSource, appSink)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Thunderbuns.Irc.Parser (parseMessageOrLine, parseAsByteString)
import Thunderbuns.Irc.Connection

data Env = Env { envServer:: Server, envLogger:: Logger} deriving (Show)

initialEnv :: IO Env
initialEnv = do
  envServer <- input auto "./config.dhall"
  envLogger <- rootLogger "thunderbuns.root" DEBUG consoleHandler
  pure Env { envServer, envLogger }


runMain :: IO ()
runMain  = do
  env <- initialEnv
  srv <- newConnection (envServer env)
  runConnection srv
