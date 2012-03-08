
module Problem3 where

import Data.Numbers.Primes

solution :: IO ()
solution = do
    print $ last (primeFactors 600851475143)
