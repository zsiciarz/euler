
module Problem6 where

solution6 :: IO ()
solution6 = do
    print $ (sum [1..100])^2 - sum (map (^2) [1..100])
