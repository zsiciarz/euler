
module Problem16 where

import Common (digits)

solution16 :: IO ()
solution16 = do
    print $ sum $ digits (2^1000 :: Integer)
