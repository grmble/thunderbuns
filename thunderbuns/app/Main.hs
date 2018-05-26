module Main where

import Control.Monad.Reader (runReaderT)
import Thunderbuns.CmdLine (initialEnv, parseCommandLine)

main :: IO ()
main = initialEnv >>= runReaderT parseCommandLine
