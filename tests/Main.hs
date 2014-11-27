module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

import Problem1 (solution1)
import Problem2 (solution2)

testSolution1 :: Test
testSolution1 = TestCase $ do
    result <- solution1
    assertEqual "test solution for problem 1" (result `mod` 100) 68

testSolution2 :: Test
testSolution2 = TestCase $ do
    result <- solution2
    assertEqual "test solution for problem 2" (result `mod` 100) 32

main :: IO ()
main = do
    c <- runTestTT $ TestList [testSolution1, testSolution2]
    when (errors c /= 0 || failures c /= 0) exitFailure
