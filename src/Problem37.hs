
module Problem37 where

import Data.List (inits, tails)
import Data.Numbers.Primes
import Common (digits, undigits)

parts :: Int -> [Int]
parts num = map undigits $ (tail $ inits s) ++ (init $ tails s) where
    s = digits num

solution37 :: IO ()
solution37 = do
    print $ sum $ take 11 [x | x <- primes, x > 7, all isPrime $ parts x]
