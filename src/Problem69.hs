-- brute force, too slow

module Problem69 where

import Data.List (maximumBy)
import Data.Ord (comparing)

phi :: Int -> Int
phi n = length $ filter ((==1) . (gcd n)) [1..n]

solution :: IO ()
solution = do
    print $ fst $ maximumBy (comparing snd) [ (n, fromIntegral n / fromIntegral (phi n)) | n <- [1..1000000] ]
