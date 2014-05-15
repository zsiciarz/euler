
module Problem52 where

import Data.List
import Common (digits)

sameDigits :: Integer -> Integer -> Bool
sameDigits m n = sort (digits m) == sort (digits n)

solution52 :: IO ()
solution52 = do
    print $ head [x | x <- [1..]
                    , sameDigits x (2*x)
                    , sameDigits x (3*x)
                    , sameDigits x (4*x)
                    , sameDigits x (5*x)
                    , sameDigits x (6*x)
                    ]
