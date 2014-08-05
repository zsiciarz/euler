
module Problem34 where

import Common (digits, factorial)

facSum :: Int -> Int
facSum = sum . map factorial . digits

solution34 :: IO ()
solution34 = do
    print $ sum [ x | x <- [3..2540160], x == facSum x]
