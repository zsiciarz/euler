module Problem14 where

import qualified Data.Vector.Unboxed as V

collatzLength :: Int -> Int
collatzLength = (map collatz [0..] !!) where
    collatz 1 = 1
    collatz n = 1 + collatzLength(nextValue) where
        nextValue = if even n then n `div` 2 else 3 * n + 1

lengths :: V.Vector Int
lengths = V.map collatzLength $ V.enumFromN 1 1000000

solution14 :: IO ()
solution14 = do
    print $ V.maxIndex lengths
