
module Problem6 where

solution6 :: IO Integer
solution6 = do
    return $ (sum [1..100])^2 - sum (map (^2) [1..100])
