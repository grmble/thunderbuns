module Main where

import Data.Monoid
import Thunderbuns.Process

main :: IO ()
main = do
  pid <- getPid
  putStrLn ("pid of current process is " <> show pid)
