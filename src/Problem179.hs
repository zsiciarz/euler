-- works, brute force is slow

module Problem179 where

import Common (numDivisors)

solution179 :: IO ()
solution179 = do
    let ds = map numDivisors [2..10000000]
    print $ length $ filter (\(a, b) -> a == b) $ zip ds (tail ds)
