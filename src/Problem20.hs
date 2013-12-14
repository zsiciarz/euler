
module Problem20 where

import Data.Char

import Common (factorial)

solution20 :: IO ()
solution20 = do
    print $ foldr (\x y -> (digitToInt x) + y) 0 $ show $ factorial 100
