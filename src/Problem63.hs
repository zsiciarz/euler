
module Problem63 where

powersNDigit :: Integer -> [Integer]
powersNDigit n = [p | x <- [1..9], let p = x^n, p >= 10^(n-1), p < 10^n]

solution63 :: IO ()
solution63 = do
    print $ length . concat . takeWhile (not . null) $ map powersNDigit [1..]
