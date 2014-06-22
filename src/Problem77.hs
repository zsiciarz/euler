
module Problem77 where

import Data.Numbers.Primes (primes)

primes' :: [Int]
primes' = take 100 primes

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0
ways (c:cs) = n where
    n = zipWith (+) (ways cs) (replicate c 0 ++ n)

solution77 :: IO ()
solution77 = do
    print $ 1 + (length . takeWhile (<5000) . map (ways primes' !!) $ [1..])
