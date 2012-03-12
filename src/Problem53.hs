-- works, ok

module Problem53 where

factorials :: [Integer]
factorials = scanl (*) 1 [1..100]

combinations :: Int -> Int -> Integer
combinations n r = factorials !! n `div` (factorials !! r * factorials !! (n-r))

solution :: IO ()
solution = do
    print $ length [c | n <- [1..100]
                      , r <- [1..n]
                      , let c = combinations n r
                      , c > 1000000
                      ]
