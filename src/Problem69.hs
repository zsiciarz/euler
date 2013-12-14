-- works OK, 3 seconds

module Problem69 where

import Data.List (maximumBy, nub)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import Data.Ratio

phi :: Integer -> Integer
phi 1 = 1
phi n = numerator $ (n % 1) * (product $ map (\p -> (1 - 1 % p)) $ nub $ primeFactors n)

solution69 :: IO ()
solution69 = do
    print $ fst $ maximumBy (comparing snd) [ (n, fromIntegral n / fromIntegral (phi n)) | n <- [1..1000000] ]
