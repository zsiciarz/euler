
module Problem1 where

solution1 :: IO Integer
solution1 = do
    return $ sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0 ]
