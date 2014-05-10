
module Problem46 where

import Data.Numbers.Primes (primes, isPrime)

isNaturalSquare :: Int -> Bool
isNaturalSquare x = fraction == 0 where
                    (_, fraction) = properFraction . sqrt . fromIntegral $ x

rests :: Int -> [Int]
rests x = [(x - p) `div` 2 | p <- takeWhile (<x) primes]

solution46 :: IO ()
solution46 = do
    print $ head [x | x <- [33, 35..]
                    , not $ isPrime x
                    , all (not . isNaturalSquare) $ rests x
                    ]
