
module Problem41 where

import Data.List (nub)
import Data.Numbers.Primes

isPandigital :: Integer -> Bool
isPandigital x = (length $ nub w) == length w where
                 w = show x

relevantPrimes :: [Integer]
relevantPrimes = takeWhile (<987654321) primes

pandigitals :: [Integer]
pandigitals = filter isPandigital [999999999, 999999998..900000000]
-- pandigitals = filter isPandigital [9876, 9875..9000]

solution :: IO ()
solution = do
    print $ head $ dropWhile (not . isPrime) pandigitals
