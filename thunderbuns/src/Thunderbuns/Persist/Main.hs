module Thunderbuns.Persist.Main where

import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader
import Data.Foldable (fold)
import Database.Persist (insertMany_)
import System.Log.Bunyan.Context (someException)
import System.Log.Bunyan.RIO
import Thunderbuns.Config (HasDatabasePool)
import Thunderbuns.Irc.Api (Message)
import Thunderbuns.Persist.Api (responseToMessage, withSqlBackend)
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Api as W
import qualified Thunderbuns.WS.Types as W
import UnliftIO.Exception (catch, finally)
import UnliftIO.STM (TChan, atomically, readTChan, tryReadTChan, writeTChan)

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
        andMore <- atomically $ unfoldM (tryReadTChan src)
        responses <- W.makeResponses (msg : andMore)
        let dbms = fold (responseToMessage <$> responses)
        unless (null dbms) (withSqlBackend (insertMany_ dbms))
        atomically $ for_ responses (writeTChan dst)
