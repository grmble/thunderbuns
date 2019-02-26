-- ! Helpers to control scrolling
module Thunderfront.Scroll
  ( isScrolledToTop
  , isScrolledToBottom
  , clientHeight
  , scrollHeight
  , scrollTop
  , scrollToBottom
  , scrollToTop
  , scrollIntoViewTop
  , scrollIntoView
  ) where

import Prelude

import Bonsai.DOM (Element(..), failNullOrUndefined)
import Data.Function.Uncurried (Fn2, runFn2)
import Foreign (F, readNumber)
import Foreign.Index (readProp)

foreign import primitives
  :: { setScrollTop :: Fn2 Element Number Unit
     , scrollIntoViewTop :: Fn2 Element Boolean Unit
     , scrollIntoView :: Element -> Unit
     }


isScrolledToBottom :: Element -> F Boolean
isScrolledToBottom elem = do
  sh <- scrollHeight elem
  ch <- clientHeight elem
  st <- scrollTop elem
  pure $ st == (sh - ch)

isScrolledToTop :: Element -> F Boolean
isScrolledToTop elem =
  map ((==) 0.0) (scrollTop elem)

scrollTop :: Element -> F Number
scrollTop (Element elem) =
  readProp "scrollTop" elem >>= failNullOrUndefined "scrollTop" >>=  readNumber

clientHeight :: Element -> F Number
clientHeight (Element elem) =
  readProp "clientHeight" elem >>= failNullOrUndefined "clientHeight" >>= readNumber

scrollHeight :: Element -> F Number
scrollHeight (Element elem) =
  readProp "scrollHeight" elem >>= failNullOrUndefined "scrollHeight" >>= readNumber

scrollToBottom :: Element -> F Unit
scrollToBottom elem = do
  sh <- scrollHeight elem
  ch <- clientHeight elem
  pure $ runFn2 primitives.setScrollTop elem (sh - ch)

-- | Scroll the element to the top or bottom of it's container
scrollIntoViewTop :: Boolean -> Element -> F Unit
scrollIntoViewTop atTop elem =
  pure $ runFn2 primitives.scrollIntoViewTop elem atTop


scrollIntoView :: Element -> F Unit
scrollIntoView elem =
  pure $ primitives.scrollIntoView elem

scrollToTop :: Element -> F Unit
scrollToTop elem =
  pure $ runFn2 primitives.setScrollTop elem 0.0
