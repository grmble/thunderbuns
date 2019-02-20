module Thunderbuns.Irc.Connection where

import Conduit
  ( ConduitM(..)
  , ConduitT(..)
  , (.|)
  , await
  , mapC
  , runConduit
  , yield
  )
import Control.Concurrent.Async (Async, async, waitBoth)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import Thunderbuns.Irc.Config (Server(..), connectionSettings)
import Thunderbuns.Irc.Parser (Message(..), ircLine, parseMessageOrLine)
import UnliftIO.Exception
import UnliftIO.STM
  ( TBQueue
  , TVar
  , atomically
  , newTBQueueIO
  , newTVarIO
  , readTBQueue
  , readTVar
  , writeTBQueue
  , writeTVar
  )

data Status
  = Disconnected
  | Registration
  | Connected

data Connection = Connection
  { server :: !Server
  , fromServer :: !(TBQueue (Either ByteString Message))
  , toServer :: !(TBQueue Message)
  , status :: !(TVar Status)
  }

-- | Create a new, disconnected connection
newConnection :: Server -> IO Connection
newConnection srv = do
  status <- newTVarIO Disconnected
  fromServer <- newTBQueueIO 1
  toServer <- newTBQueueIO 1
  pure Connection {server = srv, fromServer, toServer, status}

runConnection :: Connection -> IO ()
runConnection conn =
  bracket reserveConnection handleConnection releaseConnection
    --
    -- reserve the connection and start the handling threads
  where
    reserveConnection :: IO Bool
    reserveConnection =
      atomically $ do
        st <- readTVar (status conn)
        case st of
          Disconnected -> do
            writeTVar (status conn) Registration
            pure True
          _ -> pure False
    --
    -- handle the connection
    handleConnection :: Bool -> IO ()
    handleConnection isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      runTCPClient (uncurry clientSettings (connectionSettings $ server conn)) $ \tcpConn -> do
        a1 <- async $ runConduitFromServer $ appSource tcpConn
        a2 <- async $ registerConnection $ appSink tcpConn
        waitBoth a1 a2 $> ()
    registerConnection sink = do
      let loginStr =
            "NICK spamlessj\r\nUSER spamlessj 0 * :Spamless Juergen\r\n"
      runConduit $ yield loginStr .| sink
    runConduitFromServer src =
      runConduit $
      src .| CA.conduitParser parseMessageOrLine .| mapC (blubb . snd) .|
      CC.stdout
    blubb (Left x) = "Line: " <> x <> "\n"
    blubb (Right x) = "XXX: " <> (ircLine x) <> "\n"
    --
    -- release the connection
    releaseConnection :: Bool -> IO ()
    releaseConnection _ = atomically $ writeTVar (status conn) Disconnected

-- | Use a TBQueue as a conduit source.
sourceTBQueue :: MonadIO m => TBQueue a -> ConduitT () a m ()
sourceTBQueue q = do
  a <- atomically $ readTBQueue q
  yield a
  sourceTBQueue q

-- | Use a TBQueue as a conduit sink.
sinkTBQueue :: MonadIO m => TBQueue a -> ConduitT a Void m ()
sinkTBQueue q = do
  a <- await
  maybe (pure ()) (atomically . writeTBQueue q) a
