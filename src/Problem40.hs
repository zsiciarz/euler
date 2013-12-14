
module Problem40 where

import Data.Char

-- brute force approach
fraction :: String
fraction = concat [ show x | x <- [1..] ]

solution40 :: IO ()
solution40 = do
    print $ product [ digitToInt (fraction !! (i-1)) | i <- [1, 10, 100, 1000, 10000, 100000, 1000000] ]
