
module Problem73 where

import Data.Ratio

mediant :: Rational -> Rational -> Rational
mediant x y = (numerator x + numerator y) % (denominator x + denominator y)

numFractions :: Rational -> Rational -> Integer -> Integer
numFractions from to n
    | denominator m > n = 0
    | otherwise = 1 + (numFractions m to n) + (numFractions from m n)
    where m = mediant from to

solution73 :: IO ()
solution73 = do
    print $ numFractions (1 % 3) (1 % 2) 12000
