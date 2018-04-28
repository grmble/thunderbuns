module Thunderbuns.CmdLine where

import Control.Lens (view)
import Control.Monad.Reader
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative
import Thunderbuns.Config
import Thunderbuns.DB.Init
import Thunderbuns.DB.Internal
import Thunderbuns.DB.User
import Thunderbuns.Logging
import Thunderbuns.Server
import Thunderbuns.Validate
import UnliftIO.Concurrent (forkIO)
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
    runReaderT r e
  where
    commandParser =
      hsubparser
        (command
           "serve"
           (info (pure startApp) (progDesc "Run the server on PORT")) <>
         command "user" (info userParser (progDesc "Add or authorize users")) <>
         command "db" (info dbParser (progDesc "Database administration")))
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
      pure $ validateM up >>= logAuthorize
    logAuthorize x = do
      rc <- authorize x
      infoIO ("authorize: " <> T.pack (show rc))
      pure ()
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
