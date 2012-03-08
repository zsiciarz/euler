
module Problem10 where

import Data.Numbers.Primes

solution :: IO ()
solution = do
    print $ sum $ takeWhile (<2000000) primes
