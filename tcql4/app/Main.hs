module Main where

import Control.Monad.Reader (liftIO)
import Data.Conduit.Network
import Data.Foldable (for_)
import Data.String (fromString)
import qualified Data.Text.IO as TIO
import Database.CQL4
import System.Environment
import UnliftIO.Exception (bracket)

main :: IO ()
main = do
  hn <- head <$> getArgs
  runTCPClient (clientSettings 9042 $ fromString hn) $ \app -> do
    putStrLn "Have tcp socket, will initialize"
    bracket
      (connection' hexdumpLogger app)
      (runConnection' closeConnection)
      runQueryLines

runQueryLines :: Connection -> IO ()
runQueryLines conn = do
  rslt <- runConnection queryLine conn
  case rslt of
    Left err -> putStrLn ("Error: " ++ show err)
    Right _ -> pure () -- just handle the error
  -- and loop
  runQueryLines conn

queryLine :: ConnectionIO ()
queryLine = do
  line <- liftIO TIO.getLine
  rows <- executeQuery One line []
  liftIO $ for_ rows print
