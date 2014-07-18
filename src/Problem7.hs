
module Problem7 where

import Data.Numbers.Primes (primes)

solution7 :: IO ()
solution7 = do
    print $ primes !! 10000
