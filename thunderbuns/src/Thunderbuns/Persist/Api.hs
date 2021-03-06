{-# LANGUAGE DuplicateRecordFields #-}

{- | Persist Api

The "DatabasePool" is faked by a single connection in an MVar,
because SQLite does not seem to play nice with pools -
on exceptions or idle all connections get destroyed, this
checkpoints all the things while taking an exclusive lock,
and that causes all kinds of trouble with other connection attempts.

That's the theory anyway, let's see how it works.
-}
module Thunderbuns.Persist.Api where

import Control.Monad.Logger (defaultLogStr)
import Control.Monad.Reader (ReaderT)
import Data.ByteString.D64.UUID (OrderedUUID, timestamp)
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import Database.Persist
import Database.Persist.Sql (connClose)
import Database.Persist.Sqlite
import Database.Sqlite (open)
import qualified System.Log.Bunyan as Bunyan
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO
import System.Log.FastLogger (fromLogStr)
import qualified Thunderbuns.Config as C
import Thunderbuns.Persist.Gen
import Thunderbuns.Tlude
import qualified Thunderbuns.WS.Types as W
import UnliftIO.MVar (modifyMVar, newMVar, takeMVar)

openDatabase :: Bunyan r m => C.DatabaseConfig -> m C.DatabasePool
openDatabase C.DatabaseConfig {connectionURL, runMigrations} = do
  lg <- view logger >>= Bunyan.namedLogger "thunderbuns.dbpool" id
  logInfo ("opening sqlite database: " <> toText connectionURL)
  conn <- liftIO $ open connectionURL
  backend <- liftIO $ wrapConnection conn (bunyanLogFunc lg)
  when runMigrations $ do
    logInfo "running migrations"
    liftIO $ runReaderT (runMigration migrateAll) backend
  C.DatabasePool <$> newMVar backend
  where
    bunyanLogFunc lg loc logsrc loglvl logstr
      -- XXX determine debug level from loglvl
     =
      Bunyan.logDebug
        (toText $ fromLogStr $ defaultLogStr loc logsrc loglvl logstr)
        lg

closeDatabase :: Bunyan r m => C.DatabasePool -> m ()
closeDatabase (C.DatabasePool mvar) = do
  logInfo "closing sqlite database"
  liftIO $ takeMVar mvar >>= connClose

withSqlBackend ::
     (C.HasDatabasePool r, Bunyan r m) => ReaderT SqlBackend IO a -> m a
withSqlBackend persistAction =
  logDuration TRACE "withSqlBackend" $ do
    pool <- view (C.databasePool . C._DatabasePool)
    liftIO $
      modifyMVar pool $ \backend ->
        (backend, ) <$> runReaderT persistAction backend

selectChannelBefore ::
     W.Channel -> Maybe OrderedUUID -> ReaderT SqlBackend IO W.Response
selectChannelBefore channel before = do
  messages <-
    case before of
      Nothing ->
        selectList
          [MessageChannel ==. coerce channel]
          [LimitTo channelBeforeLimit, Desc MessageUuid]
      Just uuid ->
        selectList
          [MessageChannel ==. coerce channel, MessageUuid <. uuid]
          [LimitTo channelBeforeLimit, Desc MessageUuid]
  pure $ messagesToResponse ((\(Entity _ x) -> x) <$> messages)
  where
    channelBeforeLimit = 50

messagesToResponse :: [Message] -> W.Response
messagesToResponse ms = W.ChannelMessages (mkChannelMessage <$> ms)
  where
    mkChannelMessage (Message uuid from cmd channel msg) =
      W.ChannelMessage
        uuid
        (fromJust $ W.runParseFrom from)
        cmd
        (coerce channel)
        msg
        (timestamp uuid)

responseToMessage :: W.Response -> [Message]
responseToMessage (W.ChannelMessages msgs) = go <$> msgs
  where
    go W.ChannelMessage {W.uuid, W.from, W.cmd, W.channel, W.msg} =
      Message uuid (W.fromToText from) cmd (coerce channel) msg
responseToMessage _ = []
