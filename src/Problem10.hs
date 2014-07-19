
module Problem10 where

import Data.Numbers.Primes (primes)

solution10 :: IO ()
solution10 = do
    print $ sum $ takeWhile (<2000000) primes
