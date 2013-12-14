
module Problem47 where

import Data.List (nub)
import Data.Numbers.Primes (primeFactors)

countFactors :: Int -> Int
countFactors = length . nub . primeFactors

solution47 :: IO ()
solution47 = do
    print $ head [ x | x <- [1..]
                     , let y = x + 1
                     , let z = x + 2
                     , let t = x + 3
                     , countFactors x == 4
                     , countFactors y == 4
                     , countFactors z == 4
                     , countFactors t == 4
                     ]
