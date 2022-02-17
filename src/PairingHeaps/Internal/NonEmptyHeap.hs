module PairingHeaps.Internal.NonEmptyHeap
  ( -- * Nonempty Min Pairing Heap Type
    NonEmptyHeap(..)
    -- * Construction
  , singleton
  , insert
  , union
    -- * Minimum Element
  , findMin
  , deleteMin
  , deleteFindMin
    -- * Sorting
  , toNonEmpty
  , fromNonEmpty
  , heapsort
  ) where

import "base" Data.Foldable qualified as Foldable
import "base" Data.List.NonEmpty (NonEmpty(..))
import "base" Data.List.NonEmpty qualified as NonEmpty

data NonEmptyHeap a = NonEmptyHeap !a ![NonEmptyHeap a]
  deriving stock (Eq, Show)
  deriving stock (Foldable)

-- | O(1) Create the singleton 'NonEmptyHeap'
-- prop> \x -> toNonEmpty (singleton x) == x :| []
singleton :: a -> NonEmptyHeap a
singleton x = NonEmptyHeap x []
{-# INLINE singleton #-}

-- | Merge two min heaps. This is an O(1) analog of merge from mergesort.
infixl 7 `union`
union :: Ord a => NonEmptyHeap a -> NonEmptyHeap a -> NonEmptyHeap a
union t1@(NonEmptyHeap x xs) t2@(NonEmptyHeap y ys) = case x < y of
  True  -> NonEmptyHeap x (t2 : xs)
  False -> NonEmptyHeap y (t1 : ys)
{-# INLINE union #-}

-- Pairwise merge the heaps. This is the core of the deletion functions.
mergePairs :: Ord a => NonEmptyHeap a -> [NonEmptyHeap a] -> NonEmptyHeap a
mergePairs f [] = f
mergePairs f (a : []) = f `union` a
mergePairs f (a : b : bs) = f `union` a `union` mergePairs b bs
{-# INLINEABLE mergePairs #-}

-- | O(1) Insert into a 'NonEmptyHeap'.
-- >>> insert 2 (fromList [4, 3, 8])
-- fromList [2,3,4,8]
insert :: Ord a => a -> NonEmptyHeap a -> NonEmptyHeap a
insert x = union (singleton x)
{-# INLINE insert #-}

-- | O(1) Peeks the minimum element of a 'NonEmptyHeap'
-- >>> findMin (fromList [])
-- Nothing
-- >>> findMin (fromList [7, 1, 2])
-- Just 1
findMin :: NonEmptyHeap a -> a
findMin (NonEmptyHeap x _) = x
{-# INLINE findMin #-}

-- | Amortised O(lg(n)) Removes the minimum element from a 'NonEmptyHeap'
-- >>> deleteMin (fromNonEmpty (7 :| [1, 2]))
-- Just (fromList [2,7])
deleteMin :: Ord a => NonEmptyHeap a -> Maybe (NonEmptyHeap a)
deleteMin (NonEmptyHeap _ (y : ys)) = Just (mergePairs y ys)
deleteMin (NonEmptyHeap _ []) = Nothing
{-# INLINEABLE deleteMin #-}

-- | Amortised O(lg(n)) pop the minimum element from a 'NonEmptyHeap'
-- >>> deleteFindMin empty
-- Nothing
-- >>> deleteFindMin (fromList [1, 4, 0, 9])
-- Just (0,fromList [1,4,9])
deleteFindMin :: Ord a => NonEmptyHeap a -> (a, Maybe (NonEmptyHeap a))
deleteFindMin (NonEmptyHeap x (y : ys)) = (x, Just (mergePairs y ys))
deleteFindMin (NonEmptyHeap x []) = (x, Nothing)
{-# INLINEABLE deleteFindMin #-}

-- | O(n) Convert a 'NonEmptyHeap' into a sorted nonempty list.
toNonEmpty :: Ord a => NonEmptyHeap a -> NonEmpty a
toNonEmpty h1 = case deleteFindMin h1 of
  (x, h2) -> x :| go h2
  where
    go Nothing = []
    go (Just h3) = case deleteFindMin h3 of
      (x, h4) -> x : go h4

-- | O(n) Convert a nonempty list into a 'NonEmptyHeap'
fromNonEmpty :: Ord a => NonEmpty a -> NonEmptyHeap a
fromNonEmpty = Foldable.foldr1 union . NonEmpty.map singleton

-- | O(nlg(n)) A purely functional heapsort
-- >>> heapsort [10, 1, 2, 0, 7]
-- [0,1,2,7,10]
--
-- prop> \xs -> heapsort xs == sort xs
heapsort :: Ord a => NonEmpty a -> NonEmpty a
heapsort = toNonEmpty . fromNonEmpty

{-
instance (Show a, Ord a) => Show (Heap a) where
  showsPrec p h = showParen (p > 10) $ showString "fromList " . shows (toList h)
-}
