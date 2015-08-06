module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

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
import Problem23 (solution23)
import Problem24 (solution24)
import Problem25 (solution25)
import Problem26 (solution26)
import Problem27 (solution27)
import Problem28 (solution28)
import Problem29 (solution29)
import Problem30 (solution30)
import Problem31 (solution31)
import Problem32 (solution32)
import Problem33 (solution33)
import Problem34 (solution34)
import Problem35 (solution35)
import Problem36 (solution36)
import Problem37 (solution37)
import Problem38 (solution38)
import Problem39 (solution39)
import Problem40 (solution40)
import Problem41 (solution41)
import Problem42 (solution42)
import Problem43 (solution43)
import Problem44 (solution44)
import Problem45 (solution45)
import Problem46 (solution46)
import Problem47 (solution47)
import Problem48 (solution48)
import Problem49 (solution49)
import Problem50 (solution50)
import Problem51 (solution51)
import Problem52 (solution52)
import Problem53 (solution53)
import Problem54 (solution54)
import Problem55 (solution55)
import Problem56 (solution56)
import Problem57 (solution57)
import Problem58 (solution58)
import Problem59 (solution59)
import Problem60 (solution60)
import Problem61 (solution61)
import Problem62 (solution62)
import Problem63 (solution63)
import Problem64 (solution64)
import Problem65 (solution65)
import Problem67 (solution67)
import Problem69 (solution69)

makeTestCase :: Int -> IO Integer -> Integer -> TestTree
makeTestCase problemNumber solution lastDigits = testCase label $ do
    result <- solution
    assertEqual label lastDigits (abs result `mod` 100)
        where label = "test solution for problem " ++ show problemNumber

main :: IO ()
main = defaultMain $ testGroup "Tests"
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
        , makeTestCase 23 solution23 71
        , makeTestCase 24 solution24 60
        , makeTestCase 25 solution25 82
        , makeTestCase 26 solution26 83
        , makeTestCase 27 solution27 31
        , makeTestCase 28 solution28 1
        , makeTestCase 29 solution29 83
        , makeTestCase 30 solution30 39
        , makeTestCase 31 solution31 82
        , makeTestCase 32 solution32 28
        , makeTestCase 33 solution33 0
        , makeTestCase 34 solution34 30
        , makeTestCase 35 solution35 55
        , makeTestCase 36 solution36 87
        , makeTestCase 37 solution37 17
        , makeTestCase 38 solution38 54
        , makeTestCase 39 solution39 40
        , makeTestCase 40 solution40 10
        , makeTestCase 41 solution41 13
        , makeTestCase 42 solution42 62
        , makeTestCase 43 solution43 90
        , makeTestCase 44 solution44 60
        , makeTestCase 45 solution45 5
        , makeTestCase 46 solution46 77
        , makeTestCase 47 solution47 43
        , makeTestCase 48 solution48 0
        , makeTestCase 49 solution49 29
        , makeTestCase 50 solution50 51
        , makeTestCase 51 solution51 13
        , makeTestCase 52 solution52 57
        , makeTestCase 53 solution53 75
        , makeTestCase 54 solution54 76
        , makeTestCase 55 solution55 49
        , makeTestCase 56 solution56 72
        , makeTestCase 57 solution57 53
        , makeTestCase 58 solution58 41
        , makeTestCase 59 solution59 59
        , makeTestCase 60 solution60 33
        , makeTestCase 61 solution61 84
        , makeTestCase 62 solution62 83
        , makeTestCase 63 solution63 49
        , makeTestCase 64 solution64 22
        , makeTestCase 65 solution65 72
        , makeTestCase 67 solution67 73
        , makeTestCase 69 solution69 10
        ]
