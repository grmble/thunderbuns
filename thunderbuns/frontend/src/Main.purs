module Main where

import Prelude

import Bonsai (debugProgram, noDebug)
import Bonsai.Core (issueCommand)
import Bonsai.DOM (ElementId(..), document, effF, locationHash, window)
import Bonsai.Storage (getItem)
import Data.Foldable (for_)
import Data.Lens (set)
import Data.String as String
import Effect (Effect)
import Thunderfront.Controller (update)
import Thunderfront.Jwt (isExpired, readJwtToken)
import Thunderfront.Types (Msg(..), activeChannel, channelName, emptyModel)
import Thunderfront.View (viewMain)

main :: Effect Unit
main = do
  hash <- effF $ window >>= document >>= locationHash
  let model = if String.length hash > 2
              then set (activeChannel <<< channelName) (String.drop 2 hash) emptyModel
              else emptyModel
  prg <- dbgProgram (ElementId "main") update viewMain model window
  maybeJwt <- effF $ window >>= getItem "tbToken"
  for_ maybeJwt $ \str -> do
    jwt <- effF $ readJwtToken str
    exp <- isExpired jwt
    when (not exp)
      (issueCommand prg (pure $ JwtTokenMsg maybeJwt))
  pure unit
  where
    dbgProgram =
      debugProgram (noDebug
        { timing = true
        , events = true
        })
