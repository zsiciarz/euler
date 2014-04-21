-- works, brute force is slow

module Problem179 where

import Data.List (group)
import Common (numDivisors)

solution179 :: IO ()
solution179 = do
    print $ sum $ map (\l -> length l - 1) . group $ map numDivisors [2..10000000]
