
module Problem29 where

import Data.List

terms :: Int -> Int -> [Int]
terms from to = [x^y | x <- [from..to], y <- [from..to] ]

solution29 :: IO ()
solution29 = do
    print $ length $ nub $ terms 2 100
