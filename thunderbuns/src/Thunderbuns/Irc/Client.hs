{- | Irc Client connection

This is the code that handles the actual network connection to the irc server

-}
module Thunderbuns.Irc.Client where

import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import Data.Default (def)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Network.Connection as C
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO
import qualified Thunderbuns.Irc.Config as IC
import Thunderbuns.Irc.Parser (printCommand, printMessage, parseMessage)
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
import Thunderbuns.Utils (microSeconds)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Async (race_)
import UnliftIO.Exception (bracket, finally, throwString)
import UnliftIO.STM
import UnliftIO.Timeout (timeout)

-- | Connect to the host and port, optinally using tls
connect :: MonadIO m => String -> Integer -> Bool -> m C.Connection
connect host port tls = do
  ctx <- liftIO C.initConnectionContext
  let params =
        C.ConnectionParams
          host
          (fromInteger port)
          (if tls
             then Just def
             else Nothing)
          Nothing
  liftIO $ C.connectTo ctx params

clientTimeout :: Int
clientTimeout = microSeconds 60

runIrcClient ::
     forall r m. (HasIrcConnection r, Bunyan r m, MonadUnliftIO m)
  => (TChan Message -> TBQueue Command -> m ())
  -> m ()
runIrcClient registrator =
  withNamedLogger
    "thunderbuns.irc"
    id
    (bracket reserveConnection releaseConnection handleConnection)
    --
    -- connection is reserved, handle it
  where
    handleConnection :: Bool -> m ()
    handleConnection isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      logDebug "establishing connection to irc server"
      conn <- view ircConnection
      let IC.ServerConfig {IC.host, IC.port, IC.tls} = server conn
      bracket
        (connectWithTimeout (T.unpack host) (fromIntegral port) tls)
        (liftIO . C.connectionClose) $ \client -> do
        rgchan <- atomically $ dupTChan (fromServer conn)
        race_
          (srvlog "thunderbuns.irc.fromServer" $ runFromServer client)
          (race_
             (srvlog "thunderbuns.irc.toServer" $ runToServer client)
             (srvlog "thunderbuns.irc.registrator" $
              registrator rgchan (toServer conn)))
    -- connect within timeout or exception
    connectWithTimeout host port tls =
      fromJust <$> timeout clientTimeout (connect host port tls)
    getLineTimeout client =
      fromJust <$> timeout clientTimeout (C.connectionGetLine 512 client)
    --
    -- child loggers for various server threads
    srvlog :: Text -> m a -> m a
    srvlog n action = do
      conn <- view ircConnection
      withNamedLogger
        n
        (M.insert "server" (A.String $ IC.host $ server conn))
        action
    --
    -- read from the server and broadcast the parsed messages
    --
    -- i am not sure why, but this seems to hang and not react
    -- to async signals when interrupting - all the other treads go down
    runFromServer client =
      forever
        (do conn <- view ircConnection
            line <- liftIO $ chomp <$> getLineTimeout client
            let parsed =
                  Atto.parseOnly
                    (parseMessage <* Atto.endOfInput)
                    (line <> "\r\n")
            case parsed of
              Left s ->
                logRecord
                  WARN
                  (M.insert "line" (A.String $ toText line))
                  ("Can not parse line: " <> toText s)
              Right msg -> do
                logDebug (toText $ printMessage msg)
                atomically $ writeTChan (fromServer conn) msg) `finally`
      logDebug "Thread reading from IRC server terminated."
    -- from the command queue and send to server
    runToServer :: C.Connection -> m ()
    runToServer client =
      forever
        (do conn <- view ircConnection
            cmd <- atomically $ readTBQueue (toServer conn)
            logDebug (toText $ printCommand cmd)
            liftIO $ C.connectionPut client (printCommand cmd)) `finally` do
        logDebug "Thread writing to IRC server terminated."
        logDebug "HACK: close the socket to f*ck with the reading thread"
        liftIO $ C.connectionClose client

-- | Chop off a trailing CR if present
chomp :: ByteString -> ByteString
chomp bs =
  let len = B.length bs
      _last = B.last bs
      nocr =
        if _last == 0x0d
          then B.take (len - 1) bs
          else bs
   in if B.null bs
        then bs
        else nocr
