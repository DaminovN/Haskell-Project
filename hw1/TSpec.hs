module Main where

import Test.Tasty (defaultMain, testGroup)
import TTests.TTest1 (hspecTTest1)
import TTests.TTest2 (hspecTTest2)
import TTests.TTest3 (hspecTTest3)
import Control.Applicative

main :: IO ()
main = liftA3 (\x y z -> [x, y, z]) hspecTTest1 hspecTTest2 hspecTTest3 >>= \unitTests -> defaultMain $ testGroup "HW1 teacher tests" unitTests
