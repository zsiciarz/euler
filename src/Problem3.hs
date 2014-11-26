
module Problem3 where

import Data.Numbers.Primes (primeFactors)

solution3 :: IO Integer
solution3 = do
    return $ last (primeFactors 600851475143)
