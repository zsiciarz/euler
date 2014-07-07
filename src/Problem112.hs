
module Problem112 where

import qualified Data.Vector.Unboxed as V

import Common (digits)

isIncreasing :: [Int] -> Bool
isIncreasing digs = and $ zipWith (>=) digs $ tail digs

isDecreasing :: [Int] -> Bool
isDecreasing digs = and $ zipWith (<=) digs $ tail digs

isBouncy :: Int -> Bool
isBouncy x = not (isIncreasing digs || isDecreasing digs) where
    digs = digits x

numBouncies :: V.Vector Int
numBouncies = V.scanl (\acc x -> if isBouncy x then acc + 1 else acc) 0 (V.enumFromTo 0 2000000)

bouncyProportion :: Int -> Double
bouncyProportion x = fromIntegral (numBouncies V.! x) / fromIntegral x

solution112 :: IO ()
solution112 = do
    print . head . dropWhile (\x -> bouncyProportion x < 0.99) $ [21780..]
