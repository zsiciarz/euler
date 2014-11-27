module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

import Problem1 (solution1)

testSolution1 :: Test
testSolution1 = TestCase $ do
    result <- solution1
    assertEqual "test solution for problem 1" (result `mod` 100) 68

main :: IO ()
main = do
    c <- runTestTT testSolution1
    when (errors c /= 0 || failures c /= 0) exitFailure