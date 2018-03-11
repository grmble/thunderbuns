{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Lens (view)
import Control.Monad.Reader (runReaderT)
import Thunderbuns.Config
import Thunderbuns.Logging
import Thunderbuns.Server

main :: IO ()
main = do
  cfg <- readConfig
  let pri =
        if view debug cfg
          then DEBUG
          else INFO
  root <- rootLogger "thunderbuns" pri consoleHandler
  runReaderT startApp (Env cfg root)
