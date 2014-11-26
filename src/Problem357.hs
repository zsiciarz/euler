
module Problem357 where

import Data.Numbers.Primes

divisors :: Int -> [Int]
divisors x = x : divisors' 2 [1] x
    where divisors' d divs n
            | d^2 > n      = divs
            | (mod n d)==0 = if (div n d) == d
                             then divisors' (d+1) (d : divs) n
                             else divisors' (d+1) (d : (div n d) : divs) n
            | otherwise    = divisors' (d+1) divs n

possiblePrimes :: Int -> [Int] -> [Int]
possiblePrimes x divs = map (\d -> d + x `div` d) divs

solution357 :: IO Integer
solution357 = do
    return $ fromIntegral $ sum [n | n <- [1..100000000], all isPrime $ possiblePrimes n $ divisors n]
