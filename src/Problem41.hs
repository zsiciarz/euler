
module Problem41 where

import Data.List (permutations)
import Data.Numbers.Primes

pandigitals :: Int -> [Int]
pandigitals n = map read $ permutations $ take n ['1'..'9']

solution41 :: IO ()
solution41 = do
    print $ maximum $ filter isPrime $ concatMap pandigitals [3..9]
