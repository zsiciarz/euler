
module Problem16 where

import Data.Char (digitToInt)


solution :: IO ()
solution = do
    print $ sum $ map digitToInt $ show (2^1000)
