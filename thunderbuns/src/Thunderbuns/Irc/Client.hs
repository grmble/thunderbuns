{- | Irc Client connection

This is the code that handles the actual network connection to the irc server

-}
module Thunderbuns.Irc.Client where

import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Default (def)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Network.Connection as C
import System.Log.Bunyan.LogText (toText)
import System.Log.Bunyan.RIO
import qualified Thunderbuns.Irc.Config as IC
import Thunderbuns.Irc.Parser (ircCmdLine, parseMessage)
import Thunderbuns.Irc.Types
import Thunderbuns.Tlude
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Async (Concurrently(..), concurrently, runConcurrently)
import UnliftIO.Exception (bracket, finally, throwString)
import UnliftIO.STM

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

runIrcClient ::
     forall r m. (Bunyan r m, MonadUnliftIO m)
  => (TChan Message -> TBQueue Command -> m ())
  -> Connection
  -> m ()
runIrcClient registrator conn =
  withNamedLogger "thunderbuns.irc" id $
  bracket (reserveConnection conn) (releaseConnection conn) handleConnection
    --
    -- reserve the connection and start the handling threads
    --
    -- handle the connection
  where
    handleConnection :: Bool -> m ()
    handleConnection isReserved = do
      unless isReserved $
        throwString "can not reserve connection - not disconnected"
      logDebug "establishing connection to irc server"
      let IC.ServerConfig {IC.host, IC.port, IC.tls} = server conn
      bracket (connect (T.unpack host) port tls) (liftIO . C.connectionClose) $ \client -> do
        rgchan <- atomically $ dupTChan (fromServer conn)
        runConcurrently $
          (\_ _ _ -> ()) <$>
          (Concurrently $
           srvlog "thunderbuns.irc.fromServer" $ runFromServer client) <*>
          (Concurrently $ srvlog "thunderbuns.irc.toServer" $ runToServer client) <*>
          (Concurrently $
           srvlog "thunderbuns.irc.registrator" $
           registrator rgchan (toServer conn))
    --
    -- child loggers for various server threads
    srvlog :: Text -> m a -> m a
    srvlog n =
      withNamedLogger n (M.insert "server" (A.String $ IC.host $ server conn))
    --
    -- read from the server and broadcast the parsed messages
    runFromServer :: C.Connection -> m ()
    runFromServer client =
      forever
        (do line <- liftIO $ chomp <$> C.connectionGetLine 512 client
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
              Right msg -> atomically $ writeTChan (fromServer conn) msg) `finally`
      logDebug "Thread reading from IRC server terminated."
    -- from the command queue and send to server
    runToServer :: C.Connection -> m ()
    runToServer client =
      forever
        (do cmd <- atomically $ readTBQueue (toServer conn)
            liftIO $ C.connectionPut client (ircCmdLine cmd)) `finally`
      logDebug "Thread writing to IRC server terminated."

-- | Fake http client, not aware of virtual servers
--
-- will read line-to-line and print, like the irc client
-- this is to easily test with a http server
--
-- url should contain a path like /, and be url encoded
-- if it contains spaces or other funny characters.
fakeHttpClient :: B.ByteString -> C.Connection -> IO ()
fakeHttpClient path conn = concurrently fromServer toServer $> ()
  where
    fromServer =
      forever $ do
        line <- C.connectionGetLine 512 conn
        when
          (B.null line)
          (BC8.putStrLn "EMPTY LINE MEANS EOF ... or empty line")
        BC8.putStrLn (chomp line)
    toServer = C.connectionPut conn ("GET " <> path <> " HTTP/1.0\r\n\r\n")

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

ircStdinClient :: C.Connection -> IO ()
ircStdinClient conn = concurrently fromServer toServerInteractive $> ()
  where
    fromServer =
      forever $ do
        line <- C.connectionGetLine 512 conn
        BC8.putStrLn (chomp line)
    toServerInteractive =
      forever $ do
        line <- B.getLine
        C.connectionPut conn (chomp line <> "\r\n")
