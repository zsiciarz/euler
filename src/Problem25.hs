
module Problem25 where

import Common (fibs)

solution25 :: IO ()
solution25 = do
    print $ length $ takeWhile (<1000) (map (length . show) fibs)
