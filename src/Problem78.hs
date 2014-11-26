-- works for small divisors, too slow for 1000000

module Problem78 where

import Data.List (genericLength)

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0
ways (c:cs) = n where
    n = zipWith (+) (ways cs) (replicate c 0 ++ n)

solution78 :: IO Integer
solution78 = do
    return $ genericLength $ takeWhile (\t -> t `mod` 1000 /= 0) (ways [1..10000])
