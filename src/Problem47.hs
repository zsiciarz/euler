
module Problem47 where

import Data.List (nub)
import Data.Numbers.Primes (primeFactors)

countFactors :: Int -> Int
countFactors = length . nub . primeFactors

solution47 :: IO Integer
solution47 = do
    return $ fromIntegral $ head [x | x <- [1..]
                    , countFactors x == 4
                    , countFactors (x + 1) == 4
                    , countFactors (x + 2) == 4
                    , countFactors (x + 3) == 4
                    ]
