-- brute force, too long

module Problem73 where

import Data.List (sort, elemIndex)
import Data.Ratio

fractions :: [Rational]
fractions = sort [n % d | d <- [1..12000]
                        , n <- [1..d]
                        , gcd n d == 1
                        ]

solution :: IO ()
solution = do
    case elemIndex (1%3) fractions of
        Nothing -> print "Not found"
        Just i' -> case elemIndex (1%2) fractions of
            Nothing -> print "Not found"
            Just j' -> print $ j' - i'
