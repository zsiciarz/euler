
module Problem7 where

import Data.Numbers.Primes

solution :: IO ()
solution = do
    print $ primes !! 10000
