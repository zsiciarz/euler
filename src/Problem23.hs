
module Problem23 where

import qualified Data.Vector.Unboxed as V
import Common (divisors)

limit :: Int
limit = 28123

abundants :: V.Vector Bool
abundants = V.map f $ V.enumFromN 1 limit where
    f :: Int -> Bool
    f x = (2*x) < sum (divisors x)

isAbundant :: Int -> Bool
isAbundant x = abundants V.! (x - 1)

isSum :: Int -> Bool
isSum = any isAbundant . rests where
    rests n = map (n-) $ filter isAbundant [1..n `div` 2]

solution23 :: IO Integer
solution23 = do
    return $ fromIntegral $ sum $ filter (not . isSum) [1..limit]
