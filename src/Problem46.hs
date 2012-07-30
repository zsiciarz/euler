-- works

module Problem46 where

import Data.Numbers.Primes (primes, isPrime)

isNaturalSquare :: Int -> Bool
isNaturalSquare x = (snd $ properFraction root) == 0 where
                    root = sqrt $ fromIntegral x

rests :: Int -> [Int]
rests x = [(x - p) `div` 2 | p <- takeWhile (<x) primes]

solution :: IO ()
solution = do
    print $ head [x | x <- [33, 35..]
                    , not $ isPrime x
                    , all (not . isNaturalSquare) $ rests x
                    ]
