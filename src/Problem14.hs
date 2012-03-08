
module Problem14 where

import Data.List

collatz_length :: Int -> Int
collatz_length 1 = 1
collatz_length n = 1 + collatz_length(next_value)
               where next_value = if even n then n `div` 2 else 3 * n + 1

collatzLength :: Int -> Int
collatzLength = (map collatz [0..] !!) where
    collatz 1 = 1
    collatz n = 1 + collatzLength(nextValue) where
        nextValue = if even n then n `div` 2 else 3 * n + 1


lengths :: [Int]
lengths = map collatzLength [1..1000000]
-- lengths = map collatz_length [1..1000000]

solution :: IO ()
solution = do
    let l = lengths
    print $ elemIndex (maximum l) l
