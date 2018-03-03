module Main where

import Data.Conduit.Network
import Data.String (fromString)
import Database.CQL4.Connection
import Database.CQL4.Internal.Protocol
import Database.CQL4.Types
import System.Environment

main :: IO ()
main = do
  hn <- head <$> getArgs
  runTCPClient (clientSettings 9042 $ fromString hn) $ \app -> do
    rows <-
      runConnection'
        hexdumpLogger
        app
        (command
           (executeQuery (unboundQuery Quorum "select * from test.test_map")))
    print rows
