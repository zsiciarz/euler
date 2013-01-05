
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

hexagonals' :: Integral a => [a]
hexagonals' = takeFourDigits hexagonals

heptagonals' :: Integral a => [a]
heptagonals' = takeFourDigits heptagonals

octagonals' :: Integral a => [a]
octagonals' = takeFourDigits octagonals

solution :: IO ()
solution = do
    print $ length $ [(a, b, c, d, e) | a <- triangulars'
                                , b <- squares'
                                , c <- pentagonals'
                                , d <- hexagonals'
                                , e <- heptagonals'
                                ]
