module Thunderbuns.CmdLine where

import Thunderbuns.Irc.Config (Server(..))
import System.Log.Bunyan
import Dhall (input, auto)
import Thunderbuns.Irc.Connection

data Env = Env { envServer:: Server, envLogger:: Logger} deriving (Show)

initialEnv :: IO Env
initialEnv = do
  envServer <- input auto "./config.dhall"
  envLogger <- rootLogger "thunderbuns.root" DEBUG consoleHandler
  pure Env { envServer, envLogger }


runMain :: IO ()
runMain  = do
  env <- initialEnv
  srv <- newConnection (envServer env)
  runConnection srv (envLogger env)
