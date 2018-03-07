module ValuesSpec where

import Control.Monad.Reader (liftIO)
import Data.Conduit.Network
import Data.Foldable (for_)
import Data.Int
import Data.Monoid ((<>))
import qualified Data.Scientific as Scientific
import Data.String (fromString)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as V4
import Database.CQL4.Connection
import Database.CQL4.Protocol
import Database.CQL4.Types
import Database.CQL4.Values
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
        bracket
          (connection' hexdumpLogger app)
          (runConnection' closeConnection)
          f

createKSAndTables :: ConnectionIO ()
createKSAndTables = do
  let cql =
        [ "create keyspace if not exists test " <>
          "with replication = { 'class': 'SimpleStrategy', 'replication_factor': 1 } " <>
          "and durable_writes = false"
        , "use test"
        , "create table if not exists test_text (pk uuid, value text, primary key (pk))"
        , "create table if not exists test_varint (pk uuid, value varint, primary key (pk))"
        , "create table if not exists test_bigint (pk uuid, value bigint, primary key (pk))"
        , "create table if not exists test_int (pk uuid, value int, primary key (pk))"
        , "create table if not exists test_smallint (pk uuid, value smallint, primary key (pk))"
        , "create table if not exists test_tinyint (pk uuid, value tinyint, primary key (pk))"
        , "create table if not exists test_decimal (pk uuid, value decimal, primary key (pk))"
        , "create table if not exists test_boolean (pk uuid, value boolean, primary key (pk))"
        ]
  for_ cql (\c -> execute One c [])

useTest :: ConnectionIO ()
useTest = execute One "use test" []

writeValues :: IsCQLValue a => T.Text -> [a] -> ConnectionIO [(U.UUID, a)]
writeValues tname as = do
  let cql = "insert into " <> tname <> " (pk, value) values (?,?)"
  for as $ \a -> do
    uuid <- liftIO V4.nextRandom
    execute One cql [toValue uuid, toValue a]
    pure (uuid, a)

readValue :: IsCQLValue a => T.Text -> U.UUID -> ConnectionIO a
readValue tname uu = do
  let cql = "select value from " <> tname <> " where pk = ?"
  rows <- executeQuery One cql [toValue uu]
  extractSingleRow rows extract

writeAndRead ::
     (Eq a, Show a, IsCQLValue a) => Connection -> T.Text -> [a] -> IO ()
writeAndRead conn tname vs = do
  kvs <- runConnection' (useTest *> writeValues tname vs) conn
  for_ kvs $ \(k, v) -> do
    v' <- runConnection' (readValue tname k) conn
    v `shouldBe` v'

spec :: Spec
spec =
  around withDatabaseConnection $ do
    describe "setup" $
      it "create keyspace and tables" $ runConnection' createKSAndTables
    describe "text" $ do
      --{-
      it "write/read text values" $ \conn -> do
        writeAndRead conn "test_text" (["", "x"] :: [T.Text])
      it "write/read varint/integer values" $ \conn -> do
        writeAndRead conn "test_varint" ([0, 0xdeadbeef] :: [Integer])
      it "write/read bigint/int64 values" $ \conn -> do
        writeAndRead conn "test_bigint" ([0, 0xdeadbeef] :: [Int64])
      it "write/read int/int32 values" $ \conn -> do
        writeAndRead conn "test_int" ([0, 0x3ead1eef] :: [Int32])
      it "write/read smallint/int16 values" $ \conn -> do
        writeAndRead conn "test_smallint" ([0, 0x1eef] :: [Int16])
      it "write/read tinyint/integer values" $ \conn -> do
        writeAndRead conn "test_tinyint" ([0, 0x1e] :: [Int8])
      it "write/read boolean/bool values" $ \conn -> do
        writeAndRead conn "test_boolean" ([False, True])
      --}
      it "write/read decimal/scientific values" $ \conn -> do
        writeAndRead
          conn
          "test_decimal"
          ([0, 3.1415297] :: [Scientific.Scientific])
