
module Problem69 where

import Data.List (maximumBy)
import Data.Numbers.Primes (primeFactors)
import Data.Ord (comparing)
import Data.Ratio
import qualified Data.Set as S

uniqueFactors :: Integer -> [Integer]
uniqueFactors =  S.toList . S.fromList . primeFactors

-- phi(n) = n * (1 - 1/p1)(1 - 1/p2)...(1 - 1/pk) where p1, ..., pk are
-- prime factors of n; therefore n / phi(n) = 1 / (1 - 1/p1)...(1 - 1/pk)
nOverPhi :: Integer -> Rational
nOverPhi 1 = 1
nOverPhi n = 1 / product (map (\p -> 1 - 1 % p) $ uniqueFactors n)

solution69 :: IO ()
solution69 = do
    print $ fst $ maximumBy (comparing snd) [(n, nOverPhi n) | n <- [1..1000000]]
