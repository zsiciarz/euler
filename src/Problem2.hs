
module Problem2 where

import Common (fibs)

solution2 :: IO ()
solution2 = do
    print $ sum [x | x <- takeWhile (<4000000) fibs, even x]
