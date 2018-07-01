-- ! Helpers to control scrolling
module Thunderfront.Scroll where

import Prelude

import Bonsai.DOM (Element(..), failNullOrUndefined)
import Data.Function.Uncurried (Fn2, runFn2)
import Foreign (F, readNumber)
import Foreign.Index (readProp)

foreign import primitives
  :: { setScrollTop :: Fn2 Element Number Unit}

isScrolledDown :: Element -> F Boolean
isScrolledDown elem = do
  sh <- scrollHeight elem
  ch <- clientHeight elem
  st <- scrollTop elem
  pure $ st == (sh - ch)

scrollTop :: Element -> F Number
scrollTop (Element elem) =
  readProp "scrollTop" elem >>= failNullOrUndefined "scrollTop" >>=  readNumber

clientHeight :: Element -> F Number
clientHeight (Element elem) =
  readProp "clientHeight" elem >>= failNullOrUndefined "clientHeight" >>= readNumber

scrollHeight :: Element -> F Number
scrollHeight (Element elem) =
  readProp "scrollHeight" elem >>= failNullOrUndefined "scrollHeight" >>= readNumber

scrollDown :: Element -> F Unit
scrollDown elem = do
  sh <- scrollHeight elem
  ch <- clientHeight elem
  pure $ runFn2 primitives.setScrollTop elem (sh - ch)


