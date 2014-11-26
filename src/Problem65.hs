
module Problem65 where

import Data.List (intercalate)
import Data.Ratio

import Common (digits)

p :: [Integer] -> Int -> Int -> Rational
p a m n
    | m == n = 1 % (a !! m)
    | otherwise = 1 / ((a !! m) % 1 + p a (m+1) n)

convergent :: [Integer] -> Int -> Rational
convergent a n = head a % 1 + p a 1 n

fracE :: [Integer]
fracE = 2 : 1 : intercalate [1, 1] [[x] | x <- [2, 4..]]

solution65 :: IO Integer
solution65 = do
    return $ sum $ digits . numerator $ convergent fracE 99
