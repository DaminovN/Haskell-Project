module Main where

import Test.Tasty (defaultMain, testGroup)
import TTests.TTest1 (hspecTTest1)
import Control.Applicative

main :: IO ()
main = liftA (\x -> [x]) hspecTTest1 >>= \unitTests -> defaultMain $ testGroup "HW2 teacher tests" unitTests
