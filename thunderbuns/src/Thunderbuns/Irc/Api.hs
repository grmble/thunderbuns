module Thunderbuns.Irc.Api
  ( HasServerConfig(..)
  , HasIrcConnection(..)
  , TChan
  , Command
  , Message
  , IrcConnection
  , superviseIrcClient
  , sendCommand
  , newConnection
  , dupMessageChan
  ) where

import System.Log.Bunyan.RIO
import Thunderbuns.Irc.Client
import Thunderbuns.Irc.Config
import Thunderbuns.Irc.Connection
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
import Thunderbuns.Utils
import UnliftIO.STM (TChan, atomically, dupTChan)

-- | Run and supervise the IRC Client
--
-- Commands can be sent to it via 'sendCommand',
-- and messages from the server can be read from
-- the channel obtained via 'dupMessageChan'
superviseIrcClient ::
     (HasServerConfig r, HasIrcConnection r, Bunyan r m, MonadUnliftIO m)
  => m ()
superviseIrcClient =
  supervise
    "thunderbuns.irc"
    (microSeconds 30)
    (microSeconds 300)
    (runIrcClient registerConnection)

-- | A dup of the broadcast channel receiving all messages from the server
dupMessageChan :: (HasIrcConnection r, MonadReader r m, MonadIO m) => m (TChan Message)
dupMessageChan = view ircConnection >>= atomically . dupTChan . fromServer
