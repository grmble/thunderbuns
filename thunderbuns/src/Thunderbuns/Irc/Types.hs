{-# LANGUAGE TemplateHaskell #-}

module Thunderbuns.Irc.Types where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Lens (lens)
import Control.Lens.TH (makeClassy)
import Thunderbuns.Irc.Config (HasServerConfig(..), ServerConfig)
import Thunderbuns.Tlude
import UnliftIO.STM

-- | IrcServer Connection status
data Status
  = Disconnected
  | Registration
  | Connected

-- | A command to the irc server
--
-- We never send with a prefix, so Command does not have one
data Command = Command
  { cmdCmd :: !ByteString
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
--
-- This MUST have a prefix - everything from the server has one.
-- If we generate a message to simulate a server response,
-- it has one as well.
data Message = Message
  { msgPrefix :: !(Maybe ByteString)
  , msgCmd :: !Cmd
  , msgArgs :: ![ByteString]
  } deriving (Eq, Ord, Show)

-- | IrcServer connection
data IrcConnection = IrcConnection
  { server :: !ServerConfig
  , fromServer :: !(TChan Message)
  , toServer :: !(TBQueue Command)
  , status :: !(TVar Status)
  , handler :: !(TMVar ThreadId)
  }

$(makeClassy ''IrcConnection)

instance HasServerConfig IrcConnection where
  serverConfig = lens server (\s a -> s { server = a })

-- | Reserve the connection
--
-- Until it goes into status Connected, writing to the connection
-- gives an error.  Also stores a thread id for killing.
reserveConnection ::
     (HasIrcConnection r, MonadReader r m, MonadUnliftIO m) => m Bool
reserveConnection = do
  conn <- view ircConnection
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
releaseConnection ::
     (HasIrcConnection r, MonadReader r m, MonadUnliftIO m) => Bool -> m ()
releaseConnection _ = do
  conn <- view ircConnection
  atomically $ do
    _ <- takeTMVar (handler conn)
    writeTVar (status conn) Disconnected
