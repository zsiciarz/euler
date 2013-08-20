module Problem60 where

import Data.Numbers.Primes (primes, isPrime)

concatNum :: Integral a => a -> a -> a
concatNum a b = a * 10 ^ p + b where
        p = floor $ 1.0 + logBase (10.0::Double) (fromIntegral b)

checkPrimes :: Integral a => a -> a -> Bool
checkPrimes a b = (isPrime $ concatNum a b) && (isPrime $ concatNum b a)

solution :: IO ()
solution = do
    print $ checkPrimes 109 673
