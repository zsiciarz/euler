-- probably works, too slow

module Problem112 where

import Data.List (sort)

isBouncy :: Int -> Bool
isBouncy x = x' /= sx' && x' /= reverse sx' where
             x' = show x
             sx' = sort x'

bouncyProportion :: Int -> Double
bouncyProportion x = numBouncies / x' where
                     x' = fromIntegral x
                     numBouncies = fromIntegral $ length $ filter isBouncy [1..x]

solution :: IO ()
solution = do
    print $ head $ dropWhile (\x -> bouncyProportion x < 0.99) [21780..]
