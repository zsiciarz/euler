module Problem14 where

import qualified Data.Vector.Unboxed as V

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength n = 1 + collatzLength (if even n then n `div` 2 else 3 * n + 1)

lengths :: V.Vector Int
lengths = V.map collatzLength $ V.enumFromN 1 1000000

solution14 :: IO Integer
solution14 = do
    return $ fromIntegral $ 1 + V.maxIndex lengths
