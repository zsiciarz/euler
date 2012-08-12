-- works, 1.5 second

module Problem112 where

import Data.Array

import Common (digits)

isIncreasing :: [Int] -> Bool
isIncreasing digs = and $ zipWith (>=) digs $ tail digs

isDecreasing :: [Int] -> Bool
isDecreasing = isIncreasing . reverse

isBouncy :: Int -> Bool
isBouncy x = not (isIncreasing digs || isDecreasing digs) where
    digs = digits x

numBouncies :: Array Int Int
numBouncies = listArray (0,2000000) $ scanl (\acc x -> if isBouncy x then acc + 1 else acc) 0 [1..]

bouncyProportion :: Int -> Double
bouncyProportion x = fromIntegral (numBouncies ! x) / fromIntegral x

solution :: IO ()
solution = do
    print $ head $ dropWhile (\x -> bouncyProportion x < 0.99) [21780..]
