
module Problem7 where

import Data.Numbers.Primes

solution7 :: IO ()
solution7 = do
    print $ primes !! 10000
