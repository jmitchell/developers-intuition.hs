module Main where

import qualified Coin
import qualified BST

import Control.Monad
import Test.QuickCheck
import System.Exit

prop_Reverse :: [Int] -> Property
prop_Reverse xs =
  reverse (reverse xs) === xs

prop_Wrong :: [Int] -> Property
prop_Wrong xs =
  reverse xs === xs

main = do
  results <- sequence
    [ quickCheckResult prop_Reverse
    -- , quickCheckResult prop_Wrong
    , Coin.runTests
    , BST.runTests
    ]
  unless (all isSuccess results) exitFailure
