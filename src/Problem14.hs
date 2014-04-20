module Problem14 where

import qualified Data.Vector.Unboxed as V

collatzLength :: Int -> Int -> Int
collatzLength 1 x = x
collatzLength n x = collatzLength (if even n then n `div` 2 else 3 * n + 1) (x + 1)

lengths :: V.Vector Int
lengths = V.map (`collatzLength` 1) $ V.enumFromN 1 1000000

solution14 :: IO ()
solution14 = do
    print $ 1 + V.maxIndex lengths
