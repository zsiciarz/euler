-- works, ok

module Problem53 where

import Common (factorials)

combinations :: Int -> Int -> Integer
combinations n r = factorials !! n `div` (factorials !! r * factorials !! (n-r))

solution53 :: IO ()
solution53 = do
    print $ length $ filter (>1000000) [combinations n r | n <- [1..100], r <- [1..n]]
