-- works OK, 3 seconds

module Problem69 where

import Data.List (maximumBy)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import Data.Ratio
import qualified Data.Set as S

uniqueFactors :: Integer -> [Integer]
uniqueFactors =  S.toList . S.fromList . primeFactors

phi :: Integer -> Integer
phi 1 = 1
phi n = numerator $ (n % 1) * product (map (\p -> 1 - 1 % p) $ uniqueFactors n)

solution69 :: IO ()
solution69 = do
    print $ fst $ maximumBy (comparing snd) [ (n, fromIntegral n / fromIntegral (phi n)) | n <- [1..1000000] ]
