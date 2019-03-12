module Thunderbuns.Tlude
  ( module X
  , LByteString
  , LText
  ) where

import Control.Category as X ((<<<), (>>>))
import Control.Monad as X (forever, unless, when)
import Data.ByteString as X (ByteString)
import Control.Applicative as X ((<|>))
import qualified Data.ByteString.Lazy as LB
import Data.Foldable as X (for_)
import Data.Function as X ((&))
import Data.Functor as X (($>), void)
import Data.Monoid as X ((<>))
import Data.String as X (fromString)
import Data.Text as X (Text)
import qualified Data.Text.Lazy as LT
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)
import Data.Maybe as X (fromMaybe)

type LText = LT.Text

type LByteString = LB.ByteString
