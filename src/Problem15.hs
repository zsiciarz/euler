
module Problem15 where

pascal :: [[Integer]]
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

solution :: IO ()
solution = do
    print $ pascal !! 40 !! 20
