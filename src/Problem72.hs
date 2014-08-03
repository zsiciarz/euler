
module Problem72 where

import Data.Ratio
import Data.Numbers.Primes (primeFactors)
import qualified Data.Set as S

uniqueFactors :: Integer -> [Integer]
uniqueFactors =  S.toList . S.fromList . primeFactors

phi :: Integer -> Integer
phi 1 = 1
phi n = numerator $ (n % 1) * product (map (\p -> 1 - 1 % p) $ uniqueFactors n)

fareyLength :: Integer -> Integer
fareyLength 1 = 2
fareyLength n = fareyLength (n - 1) + phi n

solution72 :: IO ()
solution72 = do
    print $ fareyLength 1000000 - 2
