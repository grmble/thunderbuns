-- | Thunderbuns Channels
--
-- You are in a mze of channels, all alike.
module Thunderbuns.Channel where

import Control.Monad.Except
import Control.Monad.Reader
import Database.CQL4
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.Exception
import Thunderbuns.Validate
import Data.Traversable

-- | Channel abstraction
class Monad m =>
      MonadChannel m
  where
  list :: m [Channel] -- ^ list of all channels
  addChannel :: V Channel -> m () -- ^ add a new channel

instance HasDbConnection r =>
         MonadChannel (ReaderT r (ExceptT ThunderbunsException IO)) where
  list = ask >>= dbConnection >>= liftIO . runConnection' go
    where
      go = do
        let cql = "select channel from tb.channel"
        rows <- executeQuery Quorum cql []
        for rows (extractRow (Channel <$> extract))
  addChannel c = ask >>= dbConnection >>= liftIO . runConnection' go
    where
      go =
        execute
          Quorum
          "insert into tb.channel (channel) values (?)"
          [TextValue (channelName (unV c))]
