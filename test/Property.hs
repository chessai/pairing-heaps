module Property (tests, ascending) where

import "base" Data.Foldable qualified as Foldable
import "base" Data.List qualified as List
import "hedgehog" Hedgehog (Property, property, forAll, (===))
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Range qualified as Range

import "pairing-heaps" PairingHeaps.Min qualified as MinHeap

import Gen qualified

tests :: IO Bool
tests = do
  Hedgehog.checkSequential $ Hedgehog.Group "MinHeap"
    [ ("heapsort is sort", prop_heapsort_is_sort)
    , ("toList ascends", prop_toList_ascends)
    ]

prop_heapsort_is_sort :: Property
prop_heapsort_is_sort = property $ do
  let lo = 3
  let hi = 150
  list <- forAll $ Gen.list (Range.constant lo hi) (Gen.int (Range.constant 0 100))
  MinHeap.heapsort list === List.sort list

prop_toList_ascends :: Property
prop_toList_ascends = property $ do
  heap <- forAll $ Gen.heap (Gen.int (Range.constant 0 100))
  Hedgehog.assert (ascending (MinHeap.toList heap))

-- Is this increasing? If so, what is the maximum?
data OrderResult a = Empty | NotIncreasing | Increasing a

ascending :: (Foldable f, Ord a) => f a -> Bool
ascending = resultIsIncreasing . Foldable.foldl' go Empty
  where
    go Empty         y = Increasing y
    go NotIncreasing _ = NotIncreasing
    go (Increasing x) y
      | x <= y    = Increasing y
      | otherwise = NotIncreasing

    resultIsIncreasing = \case
      NotIncreasing -> False
      _             -> True
