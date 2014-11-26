
module Problem27 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Numbers.Primes

formula :: Int -> Int -> Int -> Int
formula a b n = n^2 + a * n + b

primesByFormula :: Int -> Int -> [Int]
primesByFormula a b = takeWhile isPrime [formula a b n | n <- [0..] ]

pairs :: [((Int, Int), Int)]
pairs = [ ((a, b), length $ primesByFormula a b) | a <- [-999..999], b <- [-999..999] ]

solution27 :: IO Integer
solution27 = do
    return $ fromIntegral $ fst p * snd p
        where p = fst $ maximumBy (comparing snd) pairs
