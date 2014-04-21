
module Problem20 where

import Common (factorial, digits)

solution20 :: IO ()
solution20 = do
    print $ sum $ digits $ factorial 100
