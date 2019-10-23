{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- MAYBE TURN ON WARNINGS ?
module TTests.TTest1
  ( hspecTTest1
  ) where

import Data.List (sort)
import Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

hspecTTest1 :: IO TestTree
hspecTTest1 = testSpec "Block1 tests" spec

correctOrder3 :: Ord a => (a, a, a) -> (a, a, a)
correctOrder3 (a, b, c) =
  let [v1, v2, v3] = sort [a, b, c]
  in (v1, v2, v3)

testOrder3 :: (Show a, Ord a) => (a, a, a) -> Spec
testOrder3 x = it (show x) $ order3 x `shouldBe` correctOrder3 x

correctSmartReplicate :: [Int] -> [Int]
correctSmartReplicate = concatMap (\x -> replicate x x)

testSmartReplicate :: [Int] -> Spec
testSmartReplicate xs = it (show xs) $ smartReplicate xs `shouldBe` correctSmartReplicate xs

-- correctContains :: (Foldable t, Eq a) => a -> [t a] -> [t a]
-- correctContains v = filter (elem v)

-- testContains :: (Show a, Foldable t, Eq a) => a -> [t a] -> Spec
-- testContains v xs = it (show "x") $ (contains v xs) `shouldBe` (correctContains v xs)

spec :: Spec
spec = do
  describe "Order 3 elements test" $ do
    testOrder3 (2, 3, 4)
    testOrder3 (5, 2, 10)
    testOrder3 ("asd", "sw", "2")
    testOrder3 ('z', 'b', 'a')
  describe "smartReplicate test" $ do
    testSmartReplicate [1, 2, 3]
    testSmartReplicate [1, 1, 1, 12]
    testSmartReplicate [10, 0]
    testSmartReplicate [0]
    it "(2, 3, 4)" $ order3 tup1 `shouldBe` expectedTup1
  -- describe "contains test" $ do
  --   testContains 3 [[1..5], [2,0], [3,4]]
  where
    expectedTup1 = (2, 3, 4)
    tup1 = (2, 3, 4)
