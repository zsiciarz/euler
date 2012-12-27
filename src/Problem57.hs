-- works, 10 seconds

module Problem57 where

import Data.Ratio

import Common (digits)

p :: [Integer] -> Int -> Int -> Rational
p a m n
    | m == n = 1 % (a !! m)
    | otherwise = 1 / ((a !! m) % 1 + p a (m+1) n)

convergent :: [Integer] -> Int -> Rational
convergent a n = head a % 1 + p a 1 n

fracSqrt2 :: [Integer]
fracSqrt2 = 1 : repeat 2

checkDigits :: Rational -> Bool
checkDigits r = (length $ digits n) > (length $ digits d) where
                n = numerator r
                d = denominator r

solution :: IO ()
solution = do
    print $ length $ [r | n <- [1..1000]
                        , let r = convergent fracSqrt2 n
                        , checkDigits r
                        ]

