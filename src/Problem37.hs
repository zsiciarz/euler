
module Problem37 where

import Data.List (inits, tails)
import Data.Numbers.Primes

parts :: Int -> [Int]
parts num = map read $ (tail $ inits s) ++ (init $ tails s) where
    s = show num

solution37 :: IO ()
solution37 = do
    print $ sum $ take 11 [x | x <- primes, x > 7, all isPrime $ parts x]
