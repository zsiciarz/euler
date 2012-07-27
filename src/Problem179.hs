-- works, brute force is slow

module Problem179 where

import Data.List (group)

numDivisors :: Integer -> Int
numDivisors x = numDivisors' 2 2 x
    where numDivisors' d divs n
            | d^2 > n        = divs
            | n `mod` d == 0 = if n `div` d == d
                               then numDivisors' (d+1) (divs+1) n
                               else numDivisors' (d+1) (divs+2) n
            | otherwise      = numDivisors' (d+1) divs n

solution :: IO ()
solution = do
    print $ sum $ map (\l -> length l - 1) . group $ map numDivisors [2..10000000]
