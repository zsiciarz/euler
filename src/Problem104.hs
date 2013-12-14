
module Problem104 where

import Data.List (sort)
import Common (fibs)

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

solution104 :: IO ()
solution104 = do
    print $ head [n | (x, n) <- zip fibs [0..]
                    , let digits = show x
                    , isPandigital $ take 9 digits
                    , isPandigital $ take 9 $ reverse digits
                    ]
