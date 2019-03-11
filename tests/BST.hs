module BST (runTests) where

import Demo.BST

import qualified Data.List as List
import Test.QuickCheck

runTests = quickCheckResult prop_Insert''


--------------------------------------------------------------------------------

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (BST k v) where
  arbitrary = fromList <$> arbitrary

prop_Insert :: (Int, Int) -> BST Int Int -> Property
prop_Insert (k, v) t =
  label (summarize k v t) $
  toList (insert k v t)
    ===
  List.insert (k, v)
    (toList $ deleteKey k t)

summarize k v t
  | all (>= k) (keys t) = "at start"
  | all (<= k) (keys t) = "at end"
  | otherwise           = "middle"


-- Problem: How do we know the logic in summarize aligns with our
-- mental model of these tests?
--
-- Solution: Use the new `labelledExamples` function to see a simple
-- example of inputs for each of a property's labels.

{-

> labelledExamples prop_Insert
*** Found example of at start
(0,0)
[]
[(0,0)] == [(0,0)]

*** Found example of middle
(0,0)
[(-1,2),(2,-2)]
[(-1,2),(0,0),(2,-2)] == [(-1,2),(0,0),(2,-2)]

*** Found example of at end
(0,0)
[(-2,0),(-1,2)]
[(-2,0),(-1,2),(0,0)] == [(-2,0),(-1,2),(0,0)]

-}

-- Problem: The example for "at start" inserts a k-v pair into an
-- /empty/ BST, but that wasn't our intent.
--
-- Solution: Add a new label specifically for the case when the BST is
-- empty.

prop_Insert' :: (Int, Int) -> BST Int Int -> Property
prop_Insert' (k, v) t =
  label (summarize' k v t) $
  toList (insert k v t)
    ===
  List.insert (k, v)
    (toList $ deleteKey k t)

summarize' k v t
  | t == Leaf           = "empty"
  | all (>= k) (keys t) = "at start"
  | all (<= k) (keys t) = "at end"
  | otherwise           = "middle"

-- Problem: We now get the expected result for "empty", but there's
-- another peculiar case for "at start": attempting to insert a
-- duplicate key into a singleton BST. It works, but again, this isn't
-- the spirit of what we meant by "at start".
--
-- Solution: Distinguish inserts involving a new key and those
-- involving an update to an existing key.

prop_Insert'' :: (Int, Int) -> BST Int Int -> Property
prop_Insert'' (k, v) t =
  label (summarize'' k v t) $
  toList (insert k v t)
    ===
  List.insert (k, v)
    (toList $ deleteKey k t)

summarize'' _ _ Leaf = "empty"
summarize'' k _ t    = position ++ ", " ++ newOrUpdate
  where
    position
      | all (>= k) (keys t) = "at start"
      | all (<= k) (keys t) = "at end"
      | otherwise           = "middle"
    newOrUpdate = if k `elem` keys t then "update" else "new"
