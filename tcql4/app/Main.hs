module Main where

import Control.Monad.Reader (liftIO, runReaderT)
import Data.Conduit.Network
import Data.String (fromString)
import qualified Data.Text.IO as TIO
import Database.CQL4.Connection
import Database.CQL4.Internal.Protocol
import Database.CQL4.Types
import System.Environment
import UnliftIO.Exception (bracket)

main :: IO ()
main = do
  hn <- head <$> getArgs
  runTCPClient (clientSettings 9042 $ fromString hn) $ \app -> do
    putStrLn "Have tcp socket, will initialize"
    bracket
      (connection' hexdumpLogger app)
      (runReaderT closeConnection)
      (runReaderT queryLines)

queryLines :: ConnectionIO ()
queryLines = do
  line <- liftIO $ TIO.getLine
  rows <- command $ executeQuery $ unboundQuery Quorum line
  liftIO $ print rows
  -- and loop ...
  queryLines
