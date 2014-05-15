
module Problem52 where

import Control.Applicative
import Data.List
import Common (digits)

sameDigits :: Integer -> Integer -> Bool
sameDigits m n = sort (digits m) == sort (digits n)

sameDigits' :: Integer -> Integer -> Bool
sameDigits' n x = sameDigits x (n*x)

solution52 :: IO ()
solution52 = do
    print $ head [x | x <- [1..], and $ sameDigits' <$> [2..6] <*> [x]]
