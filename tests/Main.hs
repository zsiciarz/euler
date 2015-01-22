module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

import Problem1 (solution1)
import Problem2 (solution2)
import Problem3 (solution3)
import Problem4 (solution4)
import Problem5 (solution5)
import Problem6 (solution6)
import Problem7 (solution7)
import Problem8 (solution8)
import Problem9 (solution9)
import Problem10 (solution10)
import Problem11 (solution11)
import Problem12 (solution12)
import Problem13 (solution13)
import Problem14 (solution14)
import Problem15 (solution15)
import Problem16 (solution16)
import Problem17 (solution17)
import Problem18 (solution18)
import Problem19 (solution19)
import Problem20 (solution20)
import Problem21 (solution21)
import Problem22 (solution22)

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
        , makeTestCase 3 solution3 57
        , makeTestCase 4 solution4 9
        , makeTestCase 5 solution5 60
        , makeTestCase 6 solution6 50
        , makeTestCase 7 solution7 43
        , makeTestCase 8 solution8 0
        , makeTestCase 9 solution9 0
        , makeTestCase 10 solution10 22
        , makeTestCase 11 solution11 74
        , makeTestCase 12 solution12 0
        , makeTestCase 13 solution13 30
        , makeTestCase 14 solution14 99
        , makeTestCase 15 solution15 20
        , makeTestCase 16 solution16 66
        , makeTestCase 17 solution17 24
        , makeTestCase 18 solution18 74
        , makeTestCase 19 solution19 71
        , makeTestCase 20 solution20 48
        , makeTestCase 21 solution21 26
        , makeTestCase 22 solution22 82
        ]
    when (errors c /= 0 || failures c /= 0) exitFailure
