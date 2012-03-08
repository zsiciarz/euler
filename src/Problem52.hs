
module Problem52 where

import Data.List

sameDigits :: Integer -> Integer -> Bool
sameDigits m n = sort (show m) == sort (show n)

solution :: IO ()
solution = do
    print $ head [ x | x <- [1..]
                     , sameDigits x (2*x)
                     , sameDigits x (3*x)
                     , sameDigits x (4*x)
                     , sameDigits x (5*x)
                     , sameDigits x (6*x)
                     ]
