
module Problem34 where

import Common (digits, factorial)

facSum :: Int -> Int
facSum x = sum $ map factorial (digits $ fromIntegral x)

solution34 :: IO ()
solution34 = do
    print $ sum [ x | x <- [3..2540160], x == facSum x]
