-- brute force, too long

module Problem71 where

import Data.List (sort, elemIndex)
import Data.Ratio

fractions :: [Rational]
fractions = sort [n % d | d <- [1..1000000]
                        , n <- [1..d]
                        , gcd n d == 1
                        ]

solution :: IO ()
solution = do
    let i = elemIndex (3%7) fractions
    case i of
        Just index -> print $ fractions !! (index-1)
        Nothing -> print "Not found"
