module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

import Problem1 (solution1)
import Problem2 (solution2)

makeTestCase :: Int -> IO Integer -> Integer -> Test
makeTestCase problemNumber solution lastDigits = TestCase $ do
    result <- solution
    assertEqual label (result `mod` 100) lastDigits
        where label = "test solution for problem " ++ show problemNumber

testSolution1 :: Test
testSolution1 = makeTestCase 1 solution1 68

testSolution2 :: Test
testSolution2 = makeTestCase 2 solution2 32

main :: IO ()
main = do
    c <- runTestTT $ TestList [testSolution1, testSolution2]
    when (errors c /= 0 || failures c /= 0) exitFailure
