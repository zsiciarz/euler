
module Problem34 where

import Common (digits, factorial)

facSum :: Integer -> Integer
facSum x = sum $ map factorial (digits $ fromIntegral x)

solution :: IO ()
solution = do
    print $ sum [ x | x <- [3..2540160], x == facSum x]
