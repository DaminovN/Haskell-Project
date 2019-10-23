module Main where

import Test.Tasty (defaultMain, testGroup)
import TTests.TTest2p1 (hspecTTest1)
import TTests.TTest3 (hspecTTest3)
import Control.Applicative

main :: IO ()
main = liftA2 (\x y -> [x, y]) hspecTTest1 hspecTTest3 >>= \unitTests -> defaultMain $ testGroup "HW2 tests" unitTests
