
module Problem5 where

solution5 :: IO ()
solution5 = do
    print $ foldr lcm 1 [1..20]
