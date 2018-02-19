module Thunderbuns.Config.CmdLine
where

import System.Console.CmdArgs.Explicit

arguments :: Mode [(String,String)]
arguments = mode "explicit" [] "thunderbuns" (flagArg (upd "file") "FILE")
    [ flagOpt "world" ["hello","h"] (upd "world") "WHO" "World argument"
    , flagReq ["greeting","g"] (upd "greeting") "MSG" "Greeting to give"
    , flagHelpSimple (("help",""):) ]
    where upd msg x v = Right $ (msg,x):v

parseArguments :: IO [(String, String)]
parseArguments = do
  xs <- processArgs arguments
  if ("help","") `elem` xs then
      print $ helpText [] HelpFormatDefault arguments
  else
      print xs
  return xs
