module Coin (runTests) where

import Demo.Coin

import Test.QuickCheck

runTests = quickCheckResult prop_Add''''


--------------------------------------------------------------------------------
-- Simple unit tests --

testNormal =
  Coin 2 `add` Coin 2
  ===
  Just (Coin 4)

testOverflow =
  Coin maxCoinValue `add` Coin 1
  ===
  Nothing

--------------------------------------------------------------------------------
-- Simple property-based tests --

instance Arbitrary Coin where
  arbitrary =
    Coin <$> choose (0, maxCoinValue)

prop_Valid (Coin c) =
  validCoin (Coin c)

prop_Normal (Coin a) (Coin b) =
  a+b < maxCoinValue ==>
    Coin a `add` Coin b
    ===
    Just (Coin (a+b))

prop_Overflow (Coin a) (Coin b) =
  a+b > maxCoinValue ==>
    Coin a `add` Coin b
    ===
    Nothing

-- Generated inputs which violate a property's precondition are
-- discarded. We'd prefer to avoid this sort of waste.

-------------------------------------------------------------------------------
-- Properties using new types and generators which satisfy the preconditions

data Normal = Normal Coin Coin
  deriving Show

instance Arbitrary Normal where
  arbitrary = do
    Coin a <- arbitrary
    b <- choose (0, maxCoinValue-a)
    return $ Normal (Coin a) (Coin b)

prop_ValidNormal (Normal c c') =
  validCoin c && validCoin c'

prop_Normal' (Normal (Coin a) (Coin b)) =
  Coin a `add` Coin b
  ===
  Just (Coin (a+b))

data Overflow = Overflow Coin Coin
  deriving Show

instance Arbitrary Overflow where
  arbitrary = do
    Coin a <- arbitrary
    b <- choose (maxCoinValue-a+1, maxCoinValue)
    return $ Overflow (Coin a) (Coin b)

-- FAILS on `Overflow (Coin 0) (Coin 1000001)`.
--
-- The generator shouldn't choose 0 for the first coin because it
-- forces the second coin to be invalid.
prop_ValidOverflow (Overflow c c') =
  validCoin c && validCoin c'

prop_Overflow' (Overflow (Coin a) (Coin b)) =
  Coin a `add` Coin b
  ===
  Nothing

-- Creating new types, generators, and tests every time we want to
-- test a property is tedious and error-prone.


--------------------------------------------------------------------------------
-- One property test to rule them all

prop_Add (Coin a) (Coin b) =
  Coin a `add` Coin b
  ===
  if validCoin c
    then Just c
    else Nothing
  where c = Coin (a+b)

-- Problem: It's unclear how frequently the normal and overflow cases
-- are actually tested.
--
-- Solution: Use `label` to classify generated inputs and report their
-- frequencies.

prop_Add' (Coin a) (Coin b) =
  label (summarize (a+b)) $
  Coin a `add` Coin b
    ===
  if validCoin c
    then Just c
    else Nothing
  where
    c = Coin (a+b)
    summarize n
      | n <= maxCoinValue = "normal"
      | n >  maxCoinValue = "overflow"

-- Problem: When writing unit tests it's often useful to test boundary
-- conditions, but the generator for this property isn't doing that.
--
-- Solution: Measure how frequently boundary conditions are tested by
-- labelling them. Add the following case to `summarize`:
--
--    | abs (n-maxCoinValue) < 3 = "boundary"

prop_Add'' (Coin a) (Coin b) =
  label (summarize (a+b)) $
  Coin a `add` Coin b
    ===
  if validCoin c
    then Just c
    else Nothing
  where
    c = Coin (a+b)
    summarize n
      | abs (n-maxCoinValue) < 3 = "boundary"
      | n <= maxCoinValue = "normal"
      | n >  maxCoinValue = "overflow"

-- Problem: Even after 10000 tests, no boundary scenarios are
-- generated.
--
-- Solution: Make the Arbitrary instance for Coin aware of the
-- maxCoinValue.

-- Define a wrapper type so the original Arbitrary Coin instance is
-- available for comparison.
newtype BoundedCoin = BoundedCoin Coin
  deriving Show

instance Arbitrary BoundedCoin where
  arbitrary = do
    NonNegative n <- arbitrary
    BoundedCoin . Coin <$>
      oneof [ return n                  -- usually small, but possibly too large
            , return (maxCoinValue-n)   -- usually large, but possibly negative
            , choose (0, maxCoinValue)  -- in valid range
            ]

prop_Add''' (BoundedCoin (Coin a)) (BoundedCoin (Coin b)) =
  label (summarize (a+b)) $
  Coin a `add` Coin b
    ===
  if validCoin c
    then Just c
    else Nothing
  where
    c = Coin (a+b)
    summarize n
      | abs (n-maxCoinValue) < 3 = "boundary"
      | n <= maxCoinValue = "normal"
      | n >  maxCoinValue = "overflow"

-- Excellent! This discovers an off-by-one error in the code and ~4.5%
-- of the generated inputs satisfy the boundary condition.


--------------------------------------------------------------------------------
-- Verifying lower-bound on frequency of generated tests per label

-- Problem: If somebody change a shared generator for their own
-- purposes, it could result in existing labelled tests not getting
-- generated frequently enough.
--
-- Solution: Rewrite `label` to multiple `cover` calls. By default
-- breaches of the lower-bounds are *not* classified as test failures
-- because they are probabilistic. However, to check with a high
-- degree of certainty, you can use the `checkCoverage` function.

prop_Add'''' (BoundedCoin (Coin a)) (BoundedCoin (Coin b)) =
  cover 4 (abs (n-maxCoinValue) < 3) "boundary" $
  cover 40 (n <= maxCoinValue) "normal" $
  cover 40 (n > maxCoinValue) "overflow" $
  Coin a `add` Coin b
    ===
  if validCoin (Coin n)
    then Just (Coin n)
    else Nothing
  where
    n = a+b

