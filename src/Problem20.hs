
module Problem20 where

import Data.Char

import Common (factorial)

solution :: IO ()
solution = do
    print $ foldr (\x y -> (digitToInt x) + y) 0 $ show $ factorial 100
