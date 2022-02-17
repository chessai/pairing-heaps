module Gen
  ( nonEmptyHeap
  , heap

  , fullRange

    -- * Re-exports
  , Gen.list
  , Gen.int
  ) where

import "base" Data.Foldable qualified as Foldable
import "hedgehog" Hedgehog (Gen, MonadGen)
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range

import "pairing-heaps" PairingHeaps.Internal.NonEmptyHeap (NonEmptyHeap(..))
import "pairing-heaps" PairingHeaps.Min (Heap)
import "pairing-heaps" PairingHeaps.Min qualified as MinHeap

nonEmptyHeap :: Gen a -> Gen (NonEmptyHeap a)
nonEmptyHeap genA = do
  let lo = 3
  let hi = 5
  -- right-biasing here is really slow, unfortunately
  Gen.recursive Gen.choice
    -- non-recursive generators
    [ flip NonEmptyHeap [] <$> genA
    ]
    -- recursive generators
    [ NonEmptyHeap <$> genA <*> Gen.list (Range.constant lo hi) (nonEmptyHeap genA)
    ]

-- A generator for a heap which is biased toward non-empty heaps
-- as the size parameter grows
heap :: Ord a => Gen a -> Gen (Heap a)
heap genA = recursiveRightBiased
  (pure MinHeap.empty)
  (fmap (MinHeap.fromList . Foldable.toList) $ nonEmptyHeap genA)

fullRange :: (Integral a, Bounded a) => Gen a
fullRange = Gen.integral (Range.constant minBound maxBound)

-- Given a pair of generators for a type, create a generator
-- which favours the latter as the size parameter grows
recursiveRightBiased :: Gen a -> Gen a -> Gen a
recursiveRightBiased l r = Gen.sized $ \size -> do
  -- Note: I'm not sure how recursive* interact with this bias scheme,
  -- since they halve the size parameter each time the recursive branch
  -- is called. I haven't thought too hard about it.
  recursiveFrequency
    [(2, l)] -- non-recursive
    [(1 + fromIntegral size, r)] -- recursive

-- See https://github.com/hedgehogqa/haskell-hedgehog/issues/419
recursiveFrequency :: MonadGen m => [(Int, m a)] -> [(Int, m a)] -> m a
recursiveFrequency nonrecur recur =
  Gen.sized $ \n ->
    if n <= 1
      then Gen.frequency nonrecur
      else Gen.frequency $ nonrecur ++ fmap (fmap Gen.small) recur
