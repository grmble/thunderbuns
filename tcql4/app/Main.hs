module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Conduit
import Data.Conduit.Cereal
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Network
import Data.Monoid
import qualified Data.Serialize.Put as P
import Data.String (fromString)
import Data.Void
import Database.CQL4
import Database.CQL4.Types
import System.Environment

main :: IO ()
main = do
  hn <- head <$> getArgs
  runTCPClient (clientSettings 9042 $ fromString hn) $ \app
    -- putStrLn ("options command" :: [Char])
    -- runConduit $ command app options
   -> do
    putStrLn ("startup command" :: String)
    runConduit $ command app startup
    putStrLn ("execute query command" :: String)
    runConduit $
      command
        app
        (executeQuery $ unboundQuery Quorum
         -- XXX: maps crash us - see tables
         -- "select keyspace_name, table_name, crc_check_chance, default_time_to_live from system_schema.tables"
         -- peers is empty
         -- "select peer from system.peers"
        --"select durable_writes from system_schema.durable_writes"
        -- "select broadcast_address, gossip_generation, host_id, listen_address, rpc_address, schema_version from system.local"
        -- "select * from test.mylist"
        "select * from system_schema.tables"


        )
    putStrLn ("running message source" :: String)
    runConduit $ messageSource app .| CC.mapM_ print
    putStrLn ("BLUBB" :: String)

logit :: B.ByteString -> IO B.ByteString
logit bs = do
  putStrLn ("DEBUG: " <> show (B16.encode bs))
  pure bs

command :: AppData -> P.Put -> ConduitM () Void IO ()
command app p = sourcePut p .| CC.mapM logit .| appSink app

messageSource :: AppData -> ConduitM () Message IO ()
messageSource app = appSource app .| CC.mapM logit .| conduitGet2 message
