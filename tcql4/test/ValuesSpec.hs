module ValuesSpec where

import Control.Monad.Reader (liftIO, runReaderT)
import Data.Conduit.Network
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as V4
import Database.CQL4.Connection
import Database.CQL4.Internal.Protocol
import Database.CQL4.Types
import System.Environment (lookupEnv)
import Test.Hspec
import UnliftIO.Exception (bracket)

main :: IO ()
main = hspec spec

hostAndPort :: IO (Maybe (String, Int))
hostAndPort = do
  host <- lookupEnv "CASSANDRA_TEST_HOST"
  port <- lookupEnv "CASSANDRA_TEST_PORT"
  let p = maybe 9042 read port
  case host of
    Nothing -> pure Nothing
    Just h -> pure $ Just (h, p)

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection f = do
  hp <- hostAndPort
  case hp of
    Nothing ->
      putStrLn "Test skipped, CASSANDRA_TEST_HOST/CASSANDRA_TEST_PORT not set"
    Just (h, p) ->
      runTCPClient (clientSettings p $ fromString h) $ \app ->
        bracket (connection' hexdumpLogger app) (runReaderT closeConnection) f

createKSAndTables :: ConnectionIO ()
createKSAndTables = do
  let cql =
        [ "create keyspace if not exists test " <>
          "with replication = { 'class': 'SimpleStrategy', 'replication_factor': 1 } " <>
          "and durable_writes = false"
        , "use test"
        , "create table test_text (pk uuid, value text, primary key (pk))"
        , "create table test_int (pk uuid, value int, primary key (pk))"
        , "create table test_varint (pk uuid, value varint, primary key (pk))"
        , "create table test_decimal (pk uuid, value decimal, primary key (pk))"
        ]
  for_ cql (\c -> command $ executeQuery $ makeQuery One c [])
  pure ()

useTest :: ConnectionIO ()
useTest = do
  _ <- command $ executeQuery $ makeQuery One "use test" []
  pure ()

writeTextValues :: [T.Text] -> ConnectionIO [U.UUID]
writeTextValues ts = do
  useTest
  let cql = "insert into test_text (pk, value) values (?, ?)"
  for ts $ \x -> do
    uuid <- liftIO $ V4.nextRandom
    command $ executeQuery $ makeQuery One cql [UUIDValue uuid, TextValue x]
    pure uuid

spec :: Spec
spec =
  around withDatabaseConnection $ do
    describe "setup" $
      it "create keyspace and tables" $ runReaderT createKSAndTables
    describe "text" $
      it "write/read text values" $ \conn -> do
        ks <- runReaderT (writeTextValues ["", "x"]) conn
        pure ()
