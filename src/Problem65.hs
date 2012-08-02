
module Problem65 where

import Data.List (intercalate)
import Data.Ratio

digits :: Integer -> [Integer]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

p :: [Integer] -> Int -> Int -> Rational
p a m n
    | m == n = 1 % (a !! m)
    | otherwise = 1 / ((a !! m) % 1 + p a (m+1) n)

convergent :: [Integer] -> Int -> Rational
convergent a n = (a !! 0) % 1 + p a 1 n

frac_sqrt2 :: [Integer]
frac_sqrt2 = [1] ++ repeat 2

frac_e :: [Integer]
frac_e = [2, 1] ++ intercalate [1, 1] [[x] | x <- [2, 4..]]

solution :: IO ()
solution = do
    print $ sum $ digits . numerator $ convergent frac_e 99
