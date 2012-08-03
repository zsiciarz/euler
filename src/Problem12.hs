
module Problem12 where

import Data.Numbers.Primes
import Data.List (nub, tails)

import Figurate (triangulars)


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs'
                           ]

totalCombinations :: [a] -> [[a]]
totalCombinations xs = worker (length xs) xs
                where worker 0 _ = []
                      worker k xs' = combinations k xs' ++ (worker (k-1) xs')

divisors :: Int -> [Int]
divisors n = nub $ map product $ totalCombinations $ primeFactors n

countDivisors :: Int -> Int
countDivisors n = 1 + length (divisors n)

solution :: IO ()
solution = do
    print $ head $ dropWhile (\x -> countDivisors x < 500) triangulars
