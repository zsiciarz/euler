
module Problem6 where

solution :: IO ()
solution = do
    print $ (sum [1..100])^2 - sum (map (^2) [1..100])
