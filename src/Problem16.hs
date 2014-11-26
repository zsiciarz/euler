
module Problem16 where

import Common (digits)

solution16 :: IO Integer
solution16 = do
    return $ sum $ digits (2^1000 :: Integer)
