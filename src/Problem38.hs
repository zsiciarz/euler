-- works

module Problem38 where

import Data.List ( inits, sort)

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

makeNumber :: Int -> String
makeNumber x =  maxNumber . map (concatMap show) . inits $ products where
    products = zipWith (*) (repeat x) [1..]
    maxNumber = last . takeWhile (\s -> length s < 10)

solution38 :: IO ()
solution38 = do
    print $ maximum [x | x <- map makeNumber [1..999999]
                       , isPandigital x
                       ]
