
module Problem33 where

import Data.List (intersect)
import Data.Ratio

type FractionS = (String, String)

fractionStrings :: [FractionS]
fractionStrings = [ (show x, show y) | x <- [1..5], y <- [1..5] ]

toFraction :: FractionS -> Rational
toFraction (n, d) = num % denom where
            num = read n :: Integer
            denom = read d :: Integer

cancelDigit :: FractionS -> FractionS
cancelDigit (n, d) = (i, i) where
            i = n `intersect` d

solution :: IO ()
solution = do
    print $ map cancelDigit fractionStrings
