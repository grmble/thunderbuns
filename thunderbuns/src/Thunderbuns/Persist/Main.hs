module Thunderbuns.Persist.Main where

import Control.Monad.Reader
import System.Log.Bunyan.Context (someException)
import System.Log.Bunyan.RIO
import Thunderbuns.Config (HasDatabasePool)
import Thunderbuns.Persist.Api (insertResponse, withDatabasePool)
import Thunderbuns.Irc.Api (Message)
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Handler as W
import qualified Thunderbuns.WS.Types as W
import UnliftIO.Exception (catch, finally)
import UnliftIO.STM (TChan, atomically, readTChan, writeTChan)

queueMessagesForWSClients ::
     (HasDatabasePool r, Bunyan r m, MonadUnliftIO m)
  => TChan Message
  -> TChan W.Response
  -> m ()
queueMessagesForWSClients src dst =
  withNamedLogger
    "thunderbuns.persist"
    id
    (go `catch` rego `finally`
     logDebug "queueMessagesForWSClients:  terminating")
  where
    rego e =
      logRecord INFO (someException e) "Caught exception, restarting" >> go
    go =
      forever $ do
        msg <- atomically $ readTChan src
        response <- W.makeResponse msg
        withDatabasePool (insertResponse response)
        atomically $ writeTChan dst response
