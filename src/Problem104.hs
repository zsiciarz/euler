
module Problem104 where

import Data.List (sort)
import Common (fibs)

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

fibs' :: [Integer]
fibs' = drop 2749 fibs

solution104 :: IO ()
solution104 = do
    print $ head [n | (x, n) <- zip fibs' [2750..]
                    , let digits = show x
                    , isPandigital $ take 9 digits
                    , isPandigital $ take 9 $ reverse digits
                    ]
