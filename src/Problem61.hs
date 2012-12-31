
module Problem61 where

import Figurate

takeFourDigits :: Integral a => [a] -> [a]
takeFourDigits = takeWhile (< 10000) . dropWhile (< 1000)

triangulars' :: Integral a => [a]
triangulars' = takeFourDigits triangulars

squares' :: Integral a => [a]
squares' = takeFourDigits squares

pentagonals' :: Integral a => [a]
pentagonals' = takeFourDigits pentagonals


solution :: IO ()
solution = do
    print $ length $ [(a, b, c) | a <- triangulars', b <- squares', c <- pentagonals']
