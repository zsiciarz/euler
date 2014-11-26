
module Problem187 where

import Data.List (genericLength)
import Data.Numbers.Primes (primeFactors)

solution187 :: IO Integer
solution187 = do
    return . genericLength . filter (\s -> length (primeFactors s) == 2) $ [1..100000000]
