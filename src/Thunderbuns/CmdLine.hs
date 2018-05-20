module Thunderbuns.CmdLine where

import Control.Lens (view)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Options.Applicative
import Servant.Server
import Thunderbuns.Config
import Thunderbuns.DB.Init
import Thunderbuns.DB.Internal
import Thunderbuns.DB.User
import Thunderbuns.GenPS
import Thunderbuns.Logging
import Thunderbuns.Server
import Thunderbuns.Server.User
import Thunderbuns.Validate
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Exception (throwIO)
import UnliftIO.STM

parseCommandLine :: ReaderT Env IO ()
parseCommandLine =
  ReaderT $ \e -> do
    r <-
      execParser
        (info
           (commandParser <**> helper)
           (fullDesc <>
            header "thunderbuns - producing hot air for fun and profit" <>
            progDesc "Start a server or perform administrative commands"))
    x <- runExceptT $ runHandler' $ runReaderT r e
    case x of
      Left err -> print err
      Right () -> pure ()
  where
    commandParser =
      hsubparser
        (command
           "serve"
           (info (pure startApp) (progDesc "Run the server on PORT")) <>
         command "db" (info dbParser (progDesc "Database administration")) <>
         command "gen" (info genParser (progDesc "Generate code or secrets")) <>
         command "user" (info userParser (progDesc "Add or authorize users")))
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
    generateSecret = liftIO (randomSecret >>= print)
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
    nameAndPass =
      UserPass <$> argument str (metavar "USERNAME") <*>
      argument str (metavar "PASSWORD")
    addUserParser = do
      up <- nameAndPass
      pure $ validateM up >>= addUser
    authUserParser = do
      up <- nameAndPass
      pure $
        validateM up >>= Thunderbuns.Server.User.authenticate >>= liftIO . print
    dbParser =
      hsubparser
        (command
           "init"
           (info (pure initDB) (progDesc "Initialize the database")))

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
  let e = Env cfg root dbc
  _ <- forkIO $ runReaderT superviseConnection e
  pure e

runTB :: ReaderT Env Handler a -> Env -> IO (Either ServantErr a)
runTB m e = runExceptT $ runHandler' $ runReaderT m e

runTB' :: ReaderT Env Handler a -> IO a
runTB' m = do
  x <- initialEnv >>= runTB m
  case x of
    Left err -> throwIO err
    Right a -> pure a
