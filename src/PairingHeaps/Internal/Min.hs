module PairingHeaps.Internal.Min
  ( -- * Min Pairing Heap Type
    Heap(..)
    -- * Construction
  , empty
  , singleton
  , insert
  , union
    -- * Minimum Element
  , findMin
  , deleteMin
  , deleteFindMin
    -- * Sorting
  , toList
  , fromList
  , heapsort
  )
  where

import "base" Data.List.NonEmpty (NonEmpty(..))
import "base" Data.List.NonEmpty qualified as NonEmpty
import "base" GHC.Generics (Generic)

import PairingHeaps.Internal.NonEmptyHeap (NonEmptyHeap(..))
import PairingHeaps.Internal.NonEmptyHeap qualified as NonEmptyHeap

-- $setup
-- >>> import Data.List (sort)

-- | A min pairing heap
data Heap a
  = Empty
  | Pairing !(NonEmptyHeap a)
  deriving stock (Eq, Generic)
  deriving stock (Foldable)

instance (Show a, Ord a) => Show (Heap a) where
  showsPrec p h = showParen (p > 10) $ showString "fromList " . shows (toList h)

-- | O(1) The empty heap
-- >>> empty
-- fromList []
empty :: Heap a
empty = Empty
{-# INLINE empty #-}

-- | O(1) Create the singleton 'Heap'
-- prop> \x -> toList (singleton x) == [x]
singleton :: a -> Heap a
singleton = Pairing . NonEmptyHeap.singleton
{-# INLINE singleton #-}

-- | O(1) The union of two 'Heap's.
-- >>> fromList [5, 8] `union` fromList [3, 1]
-- fromList [1,3,5,8]
union :: Ord a => Heap a -> Heap a -> Heap a
union (Pairing t1) (Pairing t2) = Pairing (t1 `NonEmptyHeap.union` t2)
union t1 Empty = t1
union Empty t2 = t2
{-# INLINEABLE union #-}

-- | O(1) Insert into a 'Heap'.
-- >>> insert 2 (fromList [4, 3, 8])
-- fromList [2,3,4,8]
insert :: Ord a => a -> Heap a -> Heap a
insert x = union (singleton x)
{-# INLINE insert #-}

-- | O(1) Peeks the minimum element of a 'Heap'
-- >>> findMin (fromList [])
-- Nothing
-- >>> findMin (fromList [7, 1, 2])
-- Just 1
findMin :: Heap a -> Maybe a
findMin (Pairing h) = Just (NonEmptyHeap.findMin h)
findMin Empty = Nothing
{-# INLINE findMin #-}

-- | Amortised O(lg(n)) Removes the minimum element from a 'Heap'
-- >>> deleteMin (fromList [])
-- fromList []
-- >>> deleteMin (fromList [7, 1, 2])
-- fromList [2,7]
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Pairing h1) = case NonEmptyHeap.deleteMin h1 of
  Just h2 -> Pairing h2
  Nothing -> Empty
deleteMin Empty = Empty
{-# INLINEABLE deleteMin #-}

-- | Amortised O(lg(n)) pop the minimum element from a 'Heap'
-- >>> deleteFindMin empty
-- Nothing
-- >>> deleteFindMin (fromList [1, 4, 0, 9])
-- Just (0,fromList [1,4,9])
deleteFindMin :: Ord a => Heap a -> Maybe (a, Heap a)
deleteFindMin (Pairing h1) = case NonEmptyHeap.deleteFindMin h1 of
  (x, mh2) -> case mh2 of
    Just h2 -> Just (x, Pairing h2)
    Nothing -> Just (x, Empty)
deleteFindMin Empty = Nothing
{-# INLINEABLE deleteFindMin #-}

-- | O(n) Convert a 'Heap' into a sorted list.
toList :: Ord a => Heap a -> [a]
toList (Pairing h1) = NonEmpty.toList (NonEmptyHeap.toNonEmpty h1)
toList Empty = []

-- | O(n) Convert a list into a 'Heap'
fromList :: Ord a => [a] -> Heap a
fromList [] = Empty
fromList (x : xs) = Pairing (NonEmptyHeap.fromNonEmpty (x :| xs))

-- | O(nlg(n)) A purely functional heapsort
-- >>> heapsort [10, 1, 2, 0, 7]
-- [0,1,2,7,10]
--
-- prop> \xs -> heapsort xs == sort xs
heapsort :: Ord a => [a] -> [a]
heapsort = toList . fromList
