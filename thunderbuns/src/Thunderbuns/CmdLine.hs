module Thunderbuns.CmdLine where

import Control.Lens (view)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Data.Foldable
import Data.Semigroup ((<>))
import Options.Applicative
import Servant.Server
import Thunderbuns.Auth
import Thunderbuns.Auth.Types
import Thunderbuns.Channel
import Thunderbuns.Channel.Types
import Thunderbuns.Config
import Thunderbuns.DB.Init
import Thunderbuns.DB.Internal
import Thunderbuns.GenPS
import Thunderbuns.Logging
import Thunderbuns.Server
import Thunderbuns.Server.Auth
import Thunderbuns.Server.Types
import Thunderbuns.Validate
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (throwIO)
import UnliftIO.STM

parseCommandLine :: ReaderT Env IO ()
parseCommandLine =
  ReaderT $ \r -> do
    go <-
      execParser
        (info
           (commandParser r <**> helper)
           (fullDesc <>
            header "thunderbuns - producing hot air for fun and profit" <>
            progDesc "Start a server or perform administrative commands"))
    runTB' go r
  where
    commandParser r =
      hsubparser
        (command
           "serve"
           (info
              (pure (runReaderT startApp r))
              (progDesc "Run the server on PORT")) <>
         command "db" (info dbParser (progDesc "Database administration")) <>
         command "gen" (info genParser (progDesc "Generate code or secrets")) <>
         command "user" (info userParser (progDesc "Add or authorize users")) <>
         command
           "channel"
           (info channelParser (progDesc "Add or list channel(s)")))
    genParser :: Parser (ReaderT Env Handler ())
    genParser =
      hsubparser
        (command
           "secret"
           (info (pure generateSecret) (progDesc "Generate a random secret")) <>
         command
           "purescript"
           (info
              (pure (liftIO generatePurescript))
              (progDesc "Generate Purescript client code")))
    userParser :: Parser (ReaderT Env Handler ())
    userParser =
      hsubparser
        (command
           "add"
           (info
              addUserParser
              (progDesc "Add a user with USERNAME and PASSWORD")) <>
         command
           "auth"
           (info
              authUserParser
              (progDesc "Authorize the given USERNAME and PASSWORD")))
    channelParser :: Parser (ReaderT Env Handler ())
    channelParser =
      hsubparser
        (command
           "add"
           (info addChannelParser (progDesc "Add a channel with NAME")) <>
         command "list" (info listChannelsParser (progDesc "List channels")))
    addUserParser :: Parser (ReaderT Env Handler ())
    addUserParser = do
      up <- nameAndPass
      pure $ mapErrorT (validateM up >>= Thunderbuns.Auth.addUser)
    authUserParser :: Parser (ReaderT Env Handler ())
    authUserParser = do
      up <- nameAndPass
      pure $
        mapErrorT
          (validateM up >>= Thunderbuns.Auth.authenticate >>= liftIO . print)
    addChannelParser :: Parser (ReaderT Env Handler ())
    addChannelParser = do
      c <- Channel <$> argument str (metavar "NAME")
      pure $ mapErrorT (validateM c >>= Thunderbuns.Channel.addChannel)
    listChannelsParser :: Parser (ReaderT Env Handler ())
    listChannelsParser =
      pure $
      mapErrorT Thunderbuns.Channel.list >>= traverse_ (liftIO . print)
    dbParser :: Parser (ReaderT Env Handler ())
    dbParser =
      hsubparser
        (command
           "init"
           (info (pure initDB) (progDesc "Initialize the database")))
    generateSecret = liftIO (randomSecret >>= print)
    nameAndPass =
      UserPass <$> argument str (metavar "USERNAME") <*>
      argument str (metavar "PASSWORD")

-- | Construct initial environment from Config
--
-- This will start the db supervisor in the background
initialEnv :: IO Env
initialEnv = do
  cfg <- readConfig
  let pri =
        if view debug cfg
          then DEBUG
          else INFO
  root <- rootLogger "thunderbuns" pri consoleHandler
  dbc <- newEmptyTMVarIO
  chan <- newBroadcastTChanIO
  let e =
        Env
          { _envConfig = cfg
          , _envLogger = root
          , _envDBConn = dbc
          , _envEventChannel = chan
          }
  _ <- forkIO $ runReaderT superviseConnection e
  pure e

runTB :: ReaderT Env Handler a -> Env -> IO (Either ServantErr a)
runTB m e = runExceptT $ runHandler' $ runReaderT m e

runTB' :: ReaderT Env Handler a -> Env -> IO a
runTB' m r = do
  x <- runTB m r
  case x of
    Left err -> throwIO err
    Right a -> pure a
