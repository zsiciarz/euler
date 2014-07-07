
module Problem124 where

import Data.List (group, sortBy)
import Data.Numbers.Primes (primeFactors)
import qualified Data.Vector.Unboxed as V

rad :: Int -> Int
rad n = product $ (map head . group) $ primeFactors n

rads :: V.Vector Int
rads = V.map rad $ V.enumFromTo 1 100000

rad' :: Int -> Int
rad' n = rads V.! (n-1)

compareByRad :: Int -> Int -> Ordering
compareByRad x y
    | rx == ry = compare x y
    | otherwise = compare rx ry where
        rx = rad' x
        ry = rad' y

e :: Int -> Int
e n =  sortBy compareByRad [1..100000] !! (n-1)

solution124 :: IO ()
solution124 = do
    print $ e 10000
