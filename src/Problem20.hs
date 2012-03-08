
module Problem20 where

import Data.Char

factorial :: Integer -> Integer
factorial n = product [1..n]

solution :: IO ()
solution = do
    print $ foldr (\x y -> (digitToInt x) + y) 0 $ show $ factorial 100
