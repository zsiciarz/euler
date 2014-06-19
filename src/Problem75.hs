
module Problem75 where

import Data.List (group, sort)

type Sides = (Int, Int, Int)
sides :: [Sides]
sides = [(a, b, c) | c <- [1..], b <- [1..c], a <- [b..c], c^2 == a^2 + b^2]

maxLength = 1500000
mlimit :: Int
mlimit = ceiling $ sqrt (fromIntegral maxLength)

lengths :: [Int]
lengths =  [ l | m <- [2..mlimit]
               , n <- [1..m]
               , k <- [1..maxLength `div` 2]
               , odd (m + n)
               , gcd m n == 1
               , let l = 2 * k * (m^2 + m * n)
               , l <= maxLength
               ]

solution75 :: IO ()
solution75 = do
    print $ length $ filter (1==) $ map length $ group $ sort lengths
