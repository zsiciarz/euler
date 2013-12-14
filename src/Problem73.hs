-- brute force, takes few seconds

module Problem73 where

import Data.Ratio

fractions :: [Rational]
fractions = [frac | d <- [1..12000]
                  , n <- [1..(d-1)]
                  , gcd n d == 1
                  , let frac = n % d
                  , frac > 1 % 3
                  , frac < 1 % 2
                  ]

solution73 :: IO ()
solution73 = do
    print $ length $ fractions
