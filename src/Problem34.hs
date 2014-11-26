
module Problem34 where

import Common (digits, factorial)

facSum :: Int -> Int
facSum = sum . map factorial . digits

solution34 :: IO Integer
solution34 = do
    return $ fromIntegral $ sum [ x | x <- [3..2540160], x == facSum x]
