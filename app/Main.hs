{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Thunderbuns.Config.CmdLine
import Thunderbuns.Config (port, readConfig)
import Thunderbuns.Logging (consoleHandler, rootLogger)

-- main :: IO ()
-- main = startApp


main = do
  cfg <- readConfig
  root <- rootLogger "thunderbuns" consoleHandler
  startApp (fromInteger (port cfg)) root
