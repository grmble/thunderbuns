module Thunderbuns.DB.Internal where

import Control.Lens (view)
import Control.Monad.Reader
import Data.Streaming.Network
import qualified Data.Text.Encoding as TE
import Database.CQL4
import Thunderbuns.Config
import Thunderbuns.Config.DB (HasDbConfig, dbConfig, host, port)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (atomically, putTMVar)

-- XXX: should be in tcql4
withConnection :: HasDbConfig e => (Connection -> IO ()) -> ReaderT e IO ()
withConnection f = do
  e <- ask
  let p = fromIntegral $ view (dbConfig . port) e
  let h = TE.encodeUtf8 $ view (dbConfig . host) e
  liftIO $
    runTCPClient (clientSettingsTCP p h) $ \app ->
      bracket (connection app) (runConnection closeConnection) f

-- | Supervise the global db connection
--
-- Will put a new connection into the the Env TMVar.
-- When the socket connection breaks, take it from the TMVar again.
-- And try to reconnect ...
--
-- XXX: response parsing should be happening in the supervisor thread
superviseConnection ::
     (HasDbConfig e, HasDbConnection e) => ReaderT e IO ()
superviseConnection = do
  e <- ask
  withConnection $ \conn -> do
    atomically $ putTMVar (view dbConnectionL e) conn
    delayForever
  where
    delayForever = do
      threadDelay 1000000
      delayForever
