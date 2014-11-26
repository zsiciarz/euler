-- works, brute force is slow

module Problem179 where

import Data.List (genericLength)
import Common (numDivisors)

solution179 :: IO Integer
solution179 = do
    let ds = map numDivisors [2..10000000]
    return $ genericLength $ filter (\(a, b) -> a == b) $ zip ds (tail ds)
