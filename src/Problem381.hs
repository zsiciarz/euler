-- brute force, out of memory

module Problem381 where

import Data.Numbers.Primes (primes)

import Common (factorial)

s :: Int -> Integer
s p = (sum $ map (\k -> factorial (p - k)) [1..5]) `mod` (fromIntegral p)

solution :: IO ()
solution = do
    print $ sum $ map s $ takeWhile (<1000) $ dropWhile (<5) primes
