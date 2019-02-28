module Thunderbuns.Irc.Types where

import Data.ByteString (ByteString)
import UnliftIO.STM (TChan, TBQueue, TVar, TMVar)
import Control.Concurrent (ThreadId)
import Thunderbuns.Irc.Config (ServerConfig)

-- | IrcServer connection
data Connection = Connection
  { server :: !ServerConfig
  , fromServer :: !(TChan Message)
  , toServer :: !(TBQueue Command)
  , status :: !(TVar Status)
  , handler :: !(TMVar ThreadId)
  }

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
