
module Problem51 where

import Data.List (nub, (\\))
import Data.Numbers.Primes (primes, isPrime)

import Common (digits, undigits)

replaceDigit :: [Int] -> Int -> Int -> [Int]
replaceDigit digs digit replacement = [if d == digit then replacement else d | d <- digs]

findReplacements :: [Int] -> Int -> [Int]
findReplacements digs digit = [ num | r <- [0..9]
                                    , let digs' = replaceDigit digs digit r
                                    , let num = undigits digs'
                                    , last digs' /= 0
                                    , isPrime num
                                    ]

solution51 :: IO ()
solution51 = do
    print $ head [ head replacements | n <- primes
                                     , let digs = digits n
                                     , let rest = digs \\ nub digs
                                     , length rest > 0
                                     , r <- rest
                                     , let replacements = findReplacements digs r
                                     , length replacements == 8
                                     ]

