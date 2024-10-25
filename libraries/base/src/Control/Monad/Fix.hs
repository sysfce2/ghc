{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Monadic fixpoints, used for desugaring of @{-# LANGUAGE RecursiveDo #-}@.
--
-- Consider the generalized version of so-called @repmin@
-- (/replace with minimum/) problem:
-- accumulate elements of a container into a 'Monoid'
-- and modify each element using the final accumulator.
--
-- @
-- repmin
--   :: (Functor t, Foldable t, Monoid b)
--   => (a -> b) -> (a -> b -> c) -> t a -> t c
-- repmin f g as = fmap (\`g\` foldMap f as) as
-- @
--
-- The naive implementation as above makes two traversals. Can we do better
-- and achieve the goal in a single pass? It's seemingly impossible, because we would
-- have to know the future,
-- but lazy evaluation comes to the rescue:
--
-- @
-- import Data.Traversable (mapAccumR)
--
-- repmin
--   :: (Traversable t, Monoid b)
--   => (a -> b) -> (a -> b -> c) -> t a -> t c
-- repmin f g as =
--   let (b, cs) = mapAccumR (\\acc a -> (f a <> acc, g a b)) mempty as in cs
-- @
--
-- How can we check that @repmin@ indeed traverses only once?
-- Let's run it on an infinite input:
--
-- >>> import Data.Monoid (All(..))
-- >>> take 3 $ repmin All (const id) ([True, True, False] ++ undefined)
-- [All {getAll = False},All {getAll = False},All {getAll = False}]
--
-- So far so good, but can we generalise @g@ to return a monadic value @a -> b -> m c@?
-- The following does not work, complaining that @b@ is not in scope:
--
-- @
-- import Data.Traversable (mapAccumM)
--
-- repminM
--   :: (Traversable t, Monoid b, Monad m)
--   => (a -> b) -> (a -> b -> m c) -> t a -> m (t c)
-- repminM f g as = do
--   (b, cs) \<- mapAccumM (\\acc a -> (f a <> acc,) <$> g a b) mempty as
--   pure cs
-- @
--
-- To solve the riddle, let's rewrite @repmin@ via 'fix':
--
-- @
-- repmin
--   :: (Traversable t, Monoid b)
--   => (a -> b) -> (a -> b -> c) -> t a -> t c
-- repmin f g as = snd $ fix $
--   \\(b, cs) -> mapAccumR (\\acc a -> (f a <> acc, g a b)) mempty as
-- @
--
-- Now we can replace 'fix' with 'mfix' to obtain the solution:
--
-- @
-- repminM
--   :: (Traversable t, Monoid b, MonadFix m)
--   => (a -> b) -> (a -> b -> m c) -> t a -> m (t c)
-- repminM f g as = fmap snd $ mfix $
--   \\(~(b, cs)) -> mapAccumM (\\acc a -> (f a <> acc,) <$> g a b) mempty as
-- @
--
-- For example,
--
-- >>> import Data.Monoid (Sum(..))
-- >>> repminM Sum (\a b -> print a >> pure (a + getSum b)) [3, 5, 2]
-- 3
-- 5
-- 2
-- [13,15,12]
--
-- Incredibly, GHC is capable to do this transformation automatically,
-- when {-# LANGUAGE RecursiveDo #-} is enabled. Namely, the following
-- implementation of @repminM@ works (note @mdo@ instead of @do@):
--
-- @
-- {-# LANGUAGE RecursiveDo #-}
--
-- repminM
--   :: (Traversable t, Monoid b, MonadFix m)
--   => (a -> b) -> (a -> b -> m c) -> t a -> m (t c)
-- repminM f g as = mdo
--   (b, cs) \<- mapAccumM (\\acc a -> (f a <> acc,) <$> g a b) mempty as
--   pure cs
-- @
--
-- Further reading:
--
-- * GHC User’s Guide, The recursive do-notation.
-- * Haskell Wiki, <https://wiki.haskell.org/MonadFix MonadFix>.
-- * Levent Erkök, <https://leventerkok.github.io/papers/erkok-thesis.pdf Value recursion in monadic computations>, Oregon Graduate Institute, 2002.
-- * Levent Erkök, John Launchbury, <https://leventerkok.github.io/papers/recdo.pdf A recursive do for Haskell>, Haskell '02, 29-37, 2002.
-- * Richard S. Bird, <https://doi.org/10.1007/BF00264249 Using circular programs to eliminate multiple traversals of data>, Acta Informatica 21, 239-250, 1984.
-- * Jasper Van der Jeugt, <https://jaspervdj.be/posts/2023-07-22-lazy-layout.html Lazy layout>, 2023.

module Control.Monad.Fix
    (MonadFix(mfix),
     fix
     ) where

import GHC.Internal.Control.Monad.Fix
