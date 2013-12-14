--works, ~80 seconds

module Problem70 where

import Data.List (minimumBy, nub, sort)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import Data.Ratio

import Common (digits)

phi :: Integer -> Integer
phi 1 = 1
phi n = numerator $ (n % 1) * product (map (\p -> 1 - 1 % p) $ nub $ primeFactors n)

solution70 :: IO ()
solution70 = do
    print $ fst $ minimumBy (comparing snd)
        [ (n, fromIntegral n / fromIntegral p) | n <- [2..10000000]
                                               , let p = phi n
                                               , sort (digits n) == sort (digits p)
                                               ]
