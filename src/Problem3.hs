
module Problem3 where

import Data.Numbers.Primes

solution3 :: IO ()
solution3 = do
    print $ last (primeFactors 600851475143)
