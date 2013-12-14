
module Problem16 where

import Data.Char (digitToInt)


solution16 :: IO ()
solution16 = do
    print $ sum $ map digitToInt $ show (2^1000)
