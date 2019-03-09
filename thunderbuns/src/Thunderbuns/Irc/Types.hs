module Thunderbuns.Irc.Types where

import Control.Concurrent (ThreadId, myThreadId)
import Thunderbuns.Irc.Config (ServerConfig)
import Thunderbuns.Tlude
import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.STM

-- | IrcServer connection
data Connection = Connection
  { server :: !ServerConfig
  , fromServer :: !(TChan Message)
  , toServer :: !(TBQueue Command)
  , status :: !(TVar Status)
  , handler :: !(TMVar ThreadId)
  }

-- | Reserve the connection
--
-- Until it goes into status Connected, writing to the connection
-- gives an error.  Also stores a thread id for killing.
reserveConnection :: MonadUnliftIO m => Connection -> m Bool
reserveConnection conn = do
  tid <- liftIO myThreadId
  atomically $ do
    st <- readTVar (status conn)
    case st of
      Disconnected -> do
        writeTVar (status conn) Registration
        putTMVar (handler conn) tid
        pure True
      _ -> pure False

-- | Release the connection
releaseConnection :: MonadUnliftIO m => Connection -> Bool -> m ()
releaseConnection conn _ =
  atomically $ do
    _ <- takeTMVar (handler conn)
    writeTVar (status conn) Disconnected

-- | IrcServer Connection status
data Status
  = Disconnected
  | Registration
  | Connected

-- | A command to the irc server
data Command = Command
  { cmdPrefix :: !(Maybe ByteString)
  , cmdCmd :: !ByteString
  , cmdArgs :: ![ByteString]
  } deriving (Eq, Ord, Show)

-- | Well known error codes - only the ones we need ...
data Code
  = NumericCode Int
  | RplWelcome -- ^ sent after successful registration (also RplYourHost, RplCreated, RplMyInfo)
  | RplBounce -- ^ sent to suggest an alternative server
  | RplNickInUse -- ^ the nick is already used
  deriving (Eq, Ord, Show)

data Cmd
  = Response !Code
  | Cmd !ByteString
  deriving (Eq, Ord, Show)

-- | A message from the IRC server
data Message = Message
  { msgPrefix :: !(Maybe ByteString)
  , msgCmd :: !Cmd
  , msgArgs :: ![ByteString]
  } deriving (Eq, Ord, Show)
