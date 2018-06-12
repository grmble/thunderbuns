{- | Free Combinators

Combinators for working with free monads.

These are all for the Church-encoded F monad -
it's much faster than the naive Free implemenation,
and simply staying in F is easier than using `improve`.

-}
module Control.Monad.Free.Combinators where

import Control.Monad.Free.Church (F, foldF)
import Control.Monad.Reader (MonadIO, MonadReader(..))

-- | A tree of free monads.
--
-- It can contain recursive sub-trees, or leaf statements.
-- E.g. a HTML DLS
--
--    div [("class", "foo")] $ do
--      p "it's a leaf!"
--
-- The context can give additional information about the subtree.
data TreeF c f a
  = TreeF c
          (F (TreeF c f) a)
  | LeafF (f a)

instance Functor f => Functor (TreeF c f) where
  fmap h (TreeF c f) = TreeF c (h <$> f)
  fmap h (LeafF f) = LeafF (h <$> f)

-- | Interpret the tree in ReaderT r IO
--
-- The contexts from the tree nodes will combined
-- by the given function.
treeIO ::
     (MonadReader r m, MonadIO m)
  => (c -> r -> r)
  -> (forall b. f b -> m b)
  -> TreeF c f a
  -> m a
treeIO f nt (TreeF c tf) = local (f c) (foldF (treeIO f nt) tf)
treeIO _ nt (LeafF f) = nt f


-- | A sum functor for combining differnt free monads
data SumF f g a
  = LeftF (f a)
  | RightF (g a)
  deriving (Show)

instance (Functor f, Functor g) => Functor (SumF f g) where
  fmap h (LeftF f) = LeftF (h <$> f)
  fmap h (RightF g) = RightF (h <$> g)

-- | A sum type of 3 alternatives 
data Sum3F f g h a
  = Left3F (f a)
  | Middle3F (g a)
  | Right3F (h a)
  deriving (Show)

instance (Functor f, Functor g, Functor h) => Functor (Sum3F f g h) where
  fmap d (Left3F f) = Left3F (d <$> f)
  fmap d (Middle3F g) = Middle3F (d <$> g)
  fmap d (Right3F h) = Right3F (d <$> h)
