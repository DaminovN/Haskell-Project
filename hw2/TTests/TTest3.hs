{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TTests.TTest3
  ( hspecTTest3
  ) where

import Task3
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

hspecTTest3 :: IO TestTree
hspecTTest3 = testSpec "Parser Tests" specParsers

specParsers :: Spec
specParsers = do
  describe "simple parsers tests" $ do
    it "ok on empty" $ runParser ok "" `shouldBe` Just ((), "")
    it "ok on nonempty" $ runParser ok "1111" `shouldBe` Just ((), "1111")
    it "eof on empty" $ runParser eof "" `shouldBe` Just ((), "")
    it "eof on nonempty" $ runParser eof "1111" `shouldBe` Nothing
    it "satisfy on string found" $ runParser (satisfy (== '1')) "1111" `shouldBe` Just ('1', "111")
    it "satisfy on string nothing" $ runParser (satisfy (== '1')) "2111" `shouldBe` Nothing
    it "satisfy on arrays found" $
      runParser (satisfy (\x -> length x == 3)) [[1, 2, 3], [4, 5], [6, 7]] `shouldBe` Just ([1, 2, 3], [[4, 5], [6, 7]])
    it "satisfy on arrays nothing" $ runParser (satisfy (\x -> length x == 4)) [[1, 2, 3], [4, 5], [6, 7]] `shouldBe` Nothing
    it "element found" $ runParser (element [1, 3]) [[1, 3], [4], [5, 6]] `shouldBe` Just ([1, 3], [[4], [5, 6]])
    it "element nothing" $ runParser (element [1, 2, 3]) [[1, 3], [4], [5, 6]] `shouldBe` Nothing
    it "stream found" $ runParser (stream [[1, 3], [4]]) [[1, 3], [4], [5, 6]] `shouldBe` Just ([[1, 3], [4]], [[5, 6]])
    it "stream nothing" $ runParser (stream [[1, 2, 3], [4]]) [[1, 3], [4], [5, 6]] `shouldBe` Nothing
  describe "bracket sequence tests" $ do
    it "correct" $ runParser correctBracketsSeq "()as" `shouldBe` Just ("()", "as")
    it "correct with garbage in end" $
      runParser correctBracketsSeq "(())()(()()())(())as" `shouldBe` Just ("(())()(()()())(())", "as")
    it "correct prefix" $ runParser correctBracketsSeq "(()))()as" `shouldBe` Just ("(())", ")()as")
    it "no brackets" $ runParser (correctBracketsSeq <* eof) "as" `shouldBe` Nothing
    it "no correct brackets sequence" $ runParser (correctBracketsSeq<* eof) ")()as" `shouldBe` Nothing
    it "no correct brackets sequence" $ runParser (correctBracketsSeq<* eof) "(()()as" `shouldBe` Nothing
  describe "nested lists sample test" $ do
    it "ConstInt a" $ runParser parseNestedLists "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
    it "big test for unsigned and signed" $
      runParser parseNestedLists "3,       2,      3,      4,  4, -5, -6    , 07     ,      8, 3, 11111, 23123, -213" `shouldBe`
      Just ([[2, 3, 4], [-5, -6, 07, 8], [11111, 23123, -213]], "")
    it "two comma" $
      runParser parseNestedLists "3, 2, 3         ,   ,  4,      -5,   -6,     07, 8, 3,    1123, 11111, -213" `shouldBe`
      Nothing
    it "negative amount" $
      runParser parseNestedLists "-3, 2, 3         ,  4,      -5,   -6,     07, 8, 3,    13123, 11111, -213" `shouldBe`
      Nothing
    it "empty subarray" $
      runParser
        parseNestedLists
        "3,       2,      3,      4, 0, 4, -5, -6    , 07     ,      8, 3, 11111, 23123, -213" `shouldBe`
      Just ([[2, 3, 4], [], [-5, -6, 7, 8], [11111, 23123, -213]], "")
    it "empty input" $ runParser parseNestedLists "" `shouldBe` Nothing
