
module Problem51 where

import Data.Char (intToDigit)
import Data.List (nub)
import Data.Numbers.Primes (primes, isPrime)
import Numeric (showIntAtBase)

import Common (digits, undigits)

digits' :: Int -> [Int]
digits' = reverse . digits


toBinaryString :: Int -> Int -> String
toBinaryString l x = replicate padLength '0' ++ s where
                     s = showIntAtBase 2 intToDigit x ""
                     padLength = l - length s

decisions :: Int -> [[Bool]]
decisions n = map (map (=='1') . toBinaryString n) [1..(2^n - 1)]

decisionsArray :: [[[Bool]]]
decisionsArray = map decisions [1..9]

decide :: Int -> Int -> Bool -> Int
decide a b f = if f then b else a

replaceDigit :: [Int] -> Int -> [Bool] -> [Int]
replaceDigit digs digit decision = zipWith3 decide digs (repeat digit) decision

primeReplacements :: Int -> [Int]
primeReplacements n = nub $ n : [replacement | d <- [1..9]
                                       , dec <- decisionsArray !! (length digs - 1)
                                       , let replacement = undigits $ replaceDigit digs d dec
                                       , isPrime replacement
                                       ]
                 where
                    digs = digits n

solution :: IO ()
solution = do
    print $ primeReplacements 56003
