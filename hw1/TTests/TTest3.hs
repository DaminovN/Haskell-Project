{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- MAYBE TURN ON WARNINGS ?
module TTests.TTest3
  ( hspecTTest3
  ) where

import Task3p1
import Task3p3
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

hspecTTest3 :: IO TestTree
hspecTTest3 = testSpec "Block3 tests" spec

testAfterDays :: WeekDays -> Int -> WeekDays -> Spec
testAfterDays wd n ans = it (show $ show wd ++ " " ++ show n) $ (afterDays wd n) `shouldBe` ans

testNextDay :: WeekDays -> WeekDays -> Spec
testNextDay wd ans = it (show wd) $ (nextDay wd) `shouldBe` ans

testIsWeekend :: WeekDays -> Bool -> Spec
testIsWeekend wd ans = it (show wd) $ (isWeekend wd) `shouldBe` ans

testDaysToParty :: WeekDays -> Int -> Spec
testDaysToParty wd ans = it (show wd) $ (daysToParty wd) `shouldBe` ans

testAdd :: String -> Nat -> Nat -> Nat -> Spec
testAdd t x y res = it t $ (x + y) `shouldBe` res

testMul :: String -> Nat -> Nat -> Nat -> Spec
testMul t x y res = it t $ (x * y) `shouldBe` res

testSub :: String -> Nat -> Nat -> Nat -> Spec
testSub t x y res = it t $ (x - y) `shouldBe` res

spec :: Spec
spec = do
  describe "afterDays test" $ do
    testAfterDays Monday 1 Tuesday
    testAfterDays Monday 2 Wednesday
    testAfterDays Monday 3 Thursday
    testAfterDays Monday 4 Friday
    testAfterDays Monday 5 Saturday
    testAfterDays Monday 6 Sunday
    testAfterDays Monday 7 Monday
  describe "nextDay test" $ do
    testNextDay Monday Tuesday
    testNextDay Tuesday Wednesday
    testNextDay Wednesday Thursday
    testNextDay Thursday Friday
    testNextDay Friday Saturday
    testNextDay Saturday Sunday
    testNextDay Sunday Monday
  describe "isWeekend test" $ do
    testIsWeekend Monday False
    testIsWeekend Tuesday False
    testIsWeekend Wednesday False
    testIsWeekend Thursday False
    testIsWeekend Friday False
    testIsWeekend Saturday True
    testIsWeekend Sunday True
  describe "daysToParty test" $ do
    testDaysToParty Monday 4
    testDaysToParty Tuesday 3
    testDaysToParty Wednesday 2
    testDaysToParty Thursday 1
    testDaysToParty Friday 0
    testDaysToParty Saturday 6
    testDaysToParty Sunday 5
  describe "test for Nat (need show, Instance Num)" $ do
    testAdd "2 + 3" (fromInteger 2) (fromInteger 3) (fromInteger 5)
    testAdd "0 + 10" (fromInteger 0) (fromInteger 10) (fromInteger 10)
    testMul "2 * 3" (fromInteger 2) (fromInteger 3) (fromInteger 6)
    testMul "0 * 10" (fromInteger 0) (fromInteger 10) (fromInteger 0)
    testMul "1 * 4" (fromInteger 1) (fromInteger 4) (fromInteger 4)
    testSub "3 - 2" (fromInteger 3) (fromInteger 2) (fromInteger 1)
    testSub "10 - 0" (fromInteger 10) (fromInteger 0) (fromInteger 10)
    testSub "5 - 5" (fromInteger 5) (fromInteger 5) (fromInteger 0)
    it ("is fromInteger 2 == 2") $ (fromInteger 2) `shouldBe` (S $ S Z)
    it ("is natToInteget (S (S Z)) == Nat 2") $ (natToInteger (S $ S Z)) `shouldBe` 2
    it ("2 > 3") $ ((fromInteger 2) > (fromInteger 3)) `shouldBe` False
    it ("3 > 2") $ ((fromInteger 3) > (fromInteger 2)) `shouldBe` True
    it ("2 >= 3") $ ((fromInteger 2) >= (fromInteger 3)) `shouldBe` False
    it ("2 >= 2") $ ((fromInteger 2) >= (fromInteger 2)) `shouldBe` True
    it ("2 < 3") $ ((fromInteger 2) < (fromInteger 3)) `shouldBe` True
    it ("2 <= 3") $ ((fromInteger 2) <= (fromInteger 3)) `shouldBe` True
    it ("2 == 3") $ ((fromInteger 2) == (fromInteger 3)) `shouldBe` False
    it ("3 <= 3") $ ((fromInteger 3) <= (fromInteger 3)) `shouldBe` True
    it ("2 <= 3") $ ((fromInteger 2) <= (fromInteger 3)) `shouldBe` True
    it ("3 <= 2") $ ((fromInteger 3) <= (fromInteger 2)) `shouldBe` False
    it ("0 < 0") $ ((fromInteger 0) < (fromInteger 0)) `shouldBe` False

