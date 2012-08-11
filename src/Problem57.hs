-- works, 10 seconds

module Problem57 where

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

checkDigits :: Rational -> Bool
checkDigits r = (length $ digits n) > (length $ digits d) where
                n = numerator r
                d = denominator r

solution :: IO ()
solution = do
    print $ length $ [r | n <- [1..1000]
                        , let r = convergent frac_sqrt2 n
                        , checkDigits r
                        ]
