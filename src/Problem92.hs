-- works, brute force

module Problem92 where

squareDigits :: Int -> Int
squareDigits 0 = 0
squareDigits x = m^2 + squareDigits d where
                (d, m) = x `quotRem` 10

chain :: Int -> Int
chain 1 = 1
chain 89 = 89
chain n = chain $ squareDigits n

solution92 :: IO ()
solution92 = do
    print $ length $ filter (\s -> chain s == 89) [1..10000000]
