
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

findMatchingNumbers :: Integral a => a -> [a] -> [a]
findMatchingNumbers n = filter (\x -> x `div` 100 == n `mod` 100)

solution61 :: IO ()
solution61 = do
    print $ findMatchingNumbers 1271 octagonals'
