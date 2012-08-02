-- brute force, too long

module Problem73 where

import Data.List (sort)
import Data.Ratio

fractions :: [Rational]
fractions = sort [n % d | d <- [1..12000]
                        , n <- [1..(d-1)]
                        , gcd n d == 1
                        ]

solution :: IO ()
solution = do
    print $ length $ takeWhile (< 1%2) $ dropWhile (<= 1%3) fractions
