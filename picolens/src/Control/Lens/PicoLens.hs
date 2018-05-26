-- | Smallest possible Lens module
module Control.Lens.PicoLens where

import Data.Functor.Const
import Data.Functor.Identity

-- a van laarhoven lens would have the following type when using a lens library
type Lens' a b
   = forall f. Functor f =>
                 (b -> f b) -> (a -> f a)

-- | view the focus of the lens
view :: ((a1 -> Const a1 b1) -> t -> Const a2 b2) -> t -> a2
view lens x = getConst $ lens Const x

-- | modify the focus of the lens
over :: ((a1 -> Identity a2) -> t -> Identity a3) -> (a1 -> a2) -> t -> a3
over lens fn x = runIdentity $ lens (Identity . fn) x
