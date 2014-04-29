
module Problem40 where

import Data.Char

-- brute force approach
fraction :: String
fraction = concatMap show [1..]

solution40 :: IO ()
solution40 = do
    print $ product [digitToInt (fraction !! i) | i <- [0, 9, 99, 999, 9999, 99999, 999999]]
