
module Problem1 where

solution1 :: IO ( )
solution1 = do
    print $ sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0 ]
