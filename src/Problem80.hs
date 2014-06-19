
module Problem80 where

import Common (digits)
import Figurate (isSquare)

-- integer-based square root
-- see: http://www.afjarvis.staff.shef.ac.uk/maths/jarvisspec02.pdf
intSqrt :: Integer -> Integer
intSqrt n = go (5 * n) 5 where
    go a b
        | b >= limit = b `div` 100
        | a >= b = go (a - b) (b + 10)
        | otherwise = go (a * 100) ((b `div` 10) * 100 + 5)
        where limit = 10^101

solution80 :: IO ()
solution80 = do
    print $ sum $ map (sum . digits . intSqrt) [i | i <- [1..100], not $ isSquare i]
