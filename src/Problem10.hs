
module Problem10 where

import Data.Numbers.Primes (primes)

solution10 :: IO Integer
solution10 = do
    return $ sum $ takeWhile (<2000000) primes
