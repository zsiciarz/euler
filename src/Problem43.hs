-- ok, works

module Problem43 where

import Data.List (permutations)

digits :: Integer -> [Integer]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

pandigitals :: [Integer]
pandigitals = filter (>1000000000) $ map read $ permutations ['0'..'9']

solution :: IO ()
solution = do
    print $ sum [ x | x <- pandigitals
                       , let d = digits x
                       , let d2 = d !! 8
                       , let d3 = d !! 7
                       , let d4 = d !! 6
                       , let d5 = d !! 5
                       , let d6 = d !! 4
                       , let d7 = d !! 3
                       , let d8 = d !! 2
                       , let d9 = d !! 1
                       , let d10 = d !! 0
                       , (100*d8 + 10*d9 + d10) `mod` 17 == 0
                       , (100*d7 + 10*d8 + d9) `mod` 13 == 0
                       , (100*d6 + 10*d7 + d8) `mod` 11 == 0
                       , (100*d5 + 10*d6 + d7) `mod` 7 == 0
                       , (100*d4 + 10*d5 + d6) `mod` 5 == 0
                       , (100*d3 + 10*d4 + d5) `mod` 3 == 0
                       , even (100*d2 + 10*d3 + d4)
                       ]
