module Thunderfront.Jwt
  ( JwtToken
  , readJwtToken
  , isExpired
  ) where

import Prelude

import Data.Either (Either(..))
import Data.JSDate (JSDate, fromTime, now)
import Data.String.Regex (Regex, regex, split)
import Data.String.Regex.Flags (RegexFlags, global)
import Effect (Effect)
import Foreign (F, Foreign, ForeignError(..), fail, readString, readNumber)
import Foreign.Index (readProp)

type JwtToken =
  { jwtSub :: String
  , jwtExp :: JSDate
  }

foreign import primDecodePart :: String -> Foreign

readJwtToken :: String -> F JwtToken
readJwtToken x = do
  r <- fRegex "\\." global
  let arr = split r x
  case arr of
    [_, payload, _] -> do
      let fp = primDecodePart payload
      sub <- readProp "jwtSub" fp >>= readString
      exp <- readProp "jwtExp" fp >>= readNumber
      pure { jwtSub: sub, jwtExp: fromTime (exp*1000.0) }
    _ -> fail $ ForeignError "jwt.token.parts"

fRegex :: String -> RegexFlags -> F Regex
fRegex s f =
  case regex s f of
    Left err -> fail $ ForeignError err
    Right r -> pure r

isExpired :: JwtToken -> Effect Boolean
isExpired jwt = do
  n <- now
  pure $ jwt.jwtExp <= n
