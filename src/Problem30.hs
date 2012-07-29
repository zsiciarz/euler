
module Problem30 where

import Common (digits)

sumPowers :: Int -> Int -> Int
sumPowers pow = sum . map (^pow) . digits

solution :: IO ()
solution = do
    print $ sum [ x | x <- [2..1234567], x == sumPowers 5 x]
