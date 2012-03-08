
module Problem187 where

import Data.Numbers.Primes

solution :: IO ()
solution = do
    print $ length $ filter (\s -> (length $ primeFactors s) == 2) [1..100000000]
