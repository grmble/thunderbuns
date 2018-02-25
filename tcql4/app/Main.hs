module Main where

import qualified Data.ByteString as B
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
    putStrLn ("startup command" :: [Char])
    runConduit $ command app startup
    putStrLn ("execute query command" :: [Char])
    runConduit $
      command app (executeQuery $ unboundQuery Quorum "select * from blubb")
    putStrLn ("running message source" :: [Char])
    runConduit $ messageSource app .| CC.mapM_ print
    putStrLn ("BLUBB" :: [Char])

logit :: B.ByteString -> IO B.ByteString
logit bs = do
  putStrLn ("DEBUG: " <> show bs)
  pure bs

command :: AppData -> P.Put -> ConduitM () Void IO ()
command app p = sourcePut p .| CC.mapM logit .| appSink app

messageSource :: AppData -> ConduitM () Message IO ()
messageSource app = appSource app .| CC.mapM logit .| conduitGet2 message
