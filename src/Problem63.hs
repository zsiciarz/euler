
module Problem63 where

import Data.List (genericLength)

powersNDigit :: Integer -> [Integer]
powersNDigit n = [p | x <- [1..9], let p = x^n, p >= 10^(n-1), p < 10^n]

solution63 :: IO Integer
solution63 = do
    return $ genericLength . concat . takeWhile (not . null) $ map powersNDigit [1..]
