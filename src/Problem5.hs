
module Problem5 where

solution5 :: IO Integer
solution5 = do
    return $ foldr lcm 1 [1..20]
