-- brute force, too long

module Problem72 where

import Data.List (sort)
import Data.Ratio

fractions :: [Rational]
fractions = sort [n % d | d <- [1..10000]
                        , n <- [1..d]
                        , gcd n d == 1
                        ]

solution :: IO ()
solution = do
    print $ length fractions
