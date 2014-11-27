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

main :: IO ()
main = do
    c <- runTestTT $ TestList
        [ makeTestCase 1 solution1 68
        , makeTestCase 2 solution2 32
        ]
    when (errors c /= 0 || failures c /= 0) exitFailure
