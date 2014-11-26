
module Problem7 where

import Data.Numbers.Primes (primes)

solution7 :: IO Integer
solution7 = do
    return $ primes !! 10000
