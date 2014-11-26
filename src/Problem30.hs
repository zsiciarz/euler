
module Problem30 where

import Common (digits)

sumPowers :: Int -> Int -> Int
sumPowers pow = sum . map (^pow) . digits

solution30 :: IO Integer
solution30 = do
    return $ fromIntegral $ sum [ x | x <- [2..1234567], x == sumPowers 5 x]
