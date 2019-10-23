{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- MAYBE TURN ON WARNINGS ?
module TTests.TTest2
  ( hspecTTest2
  ) where

import Data.List (sort)
import Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

hspecTTest2 :: IO TestTree
hspecTTest2 = testSpec "Block2 tests" spec

correctSort :: Ord t => [t] -> [t]
correctSort xs = sort xs

testMergeSort :: (Ord t, Show t) => [t] -> Spec
testMergeSort xs = it (show xs) $ (mergeSort xs) `shouldBe` (correctSort xs)

spec :: Spec
spec = do
  describe "mergeSort test" $ do
    testMergeSort [-8,1,-8,-5,-7]
