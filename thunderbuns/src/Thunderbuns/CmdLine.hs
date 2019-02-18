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
import Thunderbuns.Irc.Parser (parseMessageOrLine, parseAsText)

data Env = Env { server:: Server, logger:: Logger} deriving (Show)

initialEnv :: IO Env
initialEnv = do
  server <- input auto "./config.dhall"
  logger <- rootLogger "thunderbuns.root" DEBUG consoleHandler
  pure Env { server, logger }


runMain :: IO ()
runMain  = do
  env <- initialEnv
  logDebug (logger env) "Welcome to the jungle ..."
  let p = fromIntegral $ port $ server env
  let h = fromString $ T.unpack $ host $ server env
  runTCPClient (clientSettings p h) $ \conn -> do
    logDebug (logger env) "running output conduit"
    let loginStr = "NICK spamlessj\r\nUSER spamlessj 0 * :Spamless Juergen\r\n" :: B.ByteString
    logDebug (logger env) (T.pack $ show loginStr)
    runConduit $ yield loginStr .| appSink conn
    logDebug (logger env) "running input conduit"
    runConduit $ appSource conn .| CT.decode CT.utf8 .| CA.conduitParser parseAsText .| mapC snd .| CT.encode CT.utf8 .| CC.stdout
    logDebug (logger env) "done"

