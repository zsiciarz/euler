
module Problem77 where

import Data.List (genericLength)
import Data.Numbers.Primes (primes)

primes' :: [Int]
primes' = take 100 primes

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0
ways (c:cs) = n where
    n = zipWith (+) (ways cs) (replicate c 0 ++ n)

solution77 :: IO Integer
solution77 = do
    return $ 1 + (genericLength . takeWhile (<5000) . map (ways primes' !!) $ [1..])
