{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TTests.TTest2p1
  ( hspecTTest1
  ) where

import Task2p1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

hspecTTest1 :: IO TestTree
hspecTTest1 = testSpec "Eval of arithmetic expressions with error handling" specEval

specEval :: Spec
specEval = do
  describe "Trivial Tests" $ do
    it "ConstInt x" $ eval exprNum1 `shouldBe` Right num1
    it "+" $ eval (BinExpr Add exprNum1 exprNum2) `shouldBe` (Right $ num1 + num2)
    it "-" $ eval (BinExpr Sub exprNum1 exprNum2) `shouldBe` (Right $ num1 - num2)
    it "*" $ eval (BinExpr Mul exprNum1 exprNum2) `shouldBe` (Right $ num1 * num2)
    it "/" $ eval (BinExpr Div exprNum1 exprNum2) `shouldBe` (Right $ num1 `div` num2)
    it "^" $ eval (BinExpr Pow exprNum1 exprNum2) `shouldBe` (Right $ num1 ^ num2)
    it "DivByZero" $ eval (BinExpr Div exprNum2 (ConstInt 0)) `shouldBe` Left DivisionByZero
    it "NegPow" $ eval (BinExpr Pow exprNum1 (ConstInt (-10))) `shouldBe` Left NegativePower
  describe "NonTrivial Test" $ do
    it "Long Test" $ eval longTest `shouldBe` Right longTestVal
    it "Long DivByZero" $ eval longDivByZero `shouldBe` Left DivisionByZero
    it "Long NegPow" $ eval longNegPow `shouldBe` Left NegativePower
  where
    num1 = 20
    exprNum1 = ConstInt num1
    num2 = 10
    exprNum2 = ConstInt num2
    longTestVal = (1 + (2 * (3 ^ 4)) - 5 + (2 * (2 `div` 2 + 10 `div` 2)))
    longTest =
      BinExpr
        Add
        (BinExpr
          Sub
          (BinExpr
            Add
            (ConstInt 1)
            (BinExpr Mul (ConstInt 2) (BinExpr Pow (ConstInt 3) (ConstInt 4))))
          (ConstInt 5))
        (BinExpr
          Mul
          (ConstInt 2)
          (BinExpr
            Add
            (BinExpr Div (ConstInt 2) (ConstInt 2))
            (BinExpr Div (ConstInt 10) (ConstInt 2))))
    longDivByZero =
      BinExpr
        Add
        (BinExpr
          Sub
          (BinExpr
            Add
            (ConstInt 1)
            (BinExpr Mul (ConstInt 2) (BinExpr Pow (ConstInt 3) (ConstInt 4))))
          (ConstInt 5))
        (BinExpr
          Mul
          (ConstInt 2)
          (BinExpr
            Add
            (BinExpr Div (ConstInt 2) (ConstInt 2))
            (BinExpr Div (ConstInt 10) (ConstInt 0))))
    longNegPow =
      BinExpr
        Pow
        (BinExpr
          Add
          (BinExpr
           Sub
           (BinExpr
             Add
             (ConstInt 1)
             (BinExpr Mul (ConstInt 2) (BinExpr Pow (ConstInt 3) (ConstInt 4))))
           (ConstInt 5))
          (BinExpr
           Mul
           (ConstInt 2)
           (BinExpr
             Add
             (BinExpr Div (ConstInt 2) (ConstInt 2))
             (BinExpr Div (ConstInt 10) (ConstInt 2)))))
        (ConstInt (-10))
