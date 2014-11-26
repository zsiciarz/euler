
module Problem70 where

import Data.List (minimumBy, (\\))
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import Data.Ratio
import qualified Data.Set as S

import Common (digits)

uniqueFactors :: Integer -> [Integer]
uniqueFactors =  S.toList . S.fromList . primeFactors

phi :: Integer -> Integer
phi 1 = 1
phi n = numerator $ (n % 1) * product (map (\p -> 1 - 1 % p) $ uniqueFactors n)

solution70 :: IO Integer
solution70 = do
    return $ fst $ minimumBy (comparing snd)
        [(n, fromIntegral n / fromIntegral p) | n <- [2..10000000]
                                              , let p = phi n
                                              , null $ digits n \\ digits p
                                              ]
