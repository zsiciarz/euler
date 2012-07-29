
module Common where

digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

factorial :: Int -> Integer
factorial n = factorials !! n
