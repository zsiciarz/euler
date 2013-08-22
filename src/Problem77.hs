
module Problem77 where

import Data.Numbers.Primes (primes)

primes' :: [Int]
primes' = take 100 primes

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0
ways (c:cs) = n where
                n = zipWith (+) (ways cs) (replicate c 0 ++ n)

solution :: IO ()
solution = do
    print $ (length $ takeWhile (< 5000) $ map (\t -> ways primes' !! t) [1..]) + 1
