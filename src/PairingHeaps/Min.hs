module PairingHeaps.Min
  ( -- * Min Pairing Heap Type
    Heap
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

import PairingHeaps.Internal.Min
