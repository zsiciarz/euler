-- works, runs in 22 seconds

module Problem23 where

import Common (divisors)

limit :: Int
limit = 28123

isAbundant :: Int -> Bool
isAbundant x = (2*x) < sum (divisors x)

abundants :: [Int]
abundants = filter isAbundant [1..limit]

isSum :: Int -> Bool
isSum x = any isAbundant $ rests x where
          rests n = map (n-) $ takeWhile (<= n `div` 2) abundants

solution23 :: IO Integer
solution23 = do
    return $ fromIntegral $ sum $ filter (not . isSum) [1..limit]
