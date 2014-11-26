
module Problem2 where

import Common (fibs)

solution2 :: IO Integer
solution2 = do
    return $ sum [x | x <- takeWhile (<4000000) fibs, even x]
