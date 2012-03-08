
module Problem124 where

import Data.List (group, sortBy)
import Data.Numbers.Primes

rad :: Int -> Int
rad n = product $ (map head . group) $ primeFactors n

rads :: [Int]
rads = map rad [1..100000]

rad' :: Int -> Int
rad' n = rads !! (n-1)

compareByRad :: Int -> Int -> Ordering
compareByRad x y
               | rx == ry = compare x y
               | otherwise = compare rx ry where
                   rx = rad' x
                   ry = rad' y

e :: Int -> Int
e n =  sortBy compareByRad [1..100000] !! (n-1)

solution :: IO ()
solution = do
    print $ e 10000
