
module Problem35 where

import Data.Numbers.Primes
import Data.List

revJoin :: ([a], [a]) -> [a]
revJoin (x, y) = y ++ x

rotations :: [a] -> [[a]]
rotations l = drop 1 $ map revJoin (inits l `zip` tails l)

isCircular :: Int -> Bool
isCircular n = all isPrime (map read (rotations (show n)))

solution :: IO ()
solution = do
    print $ length [x | x <- takeWhile (<1000000) primes, isCircular x]
