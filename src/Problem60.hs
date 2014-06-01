module Problem60 where

-- possibly works, too slow

import Data.Numbers.Primes (primes, isPrime)

primes' :: Integral a => [a]
primes' = take 10000 primes

concatNum :: Integral a => a -> a -> a
concatNum a b = a * 10 ^ p + b where
        p = floor $ 1.0 + logBase (10.0::Double) (fromIntegral b)

checkPrimes :: Integral a => a -> a -> Bool
checkPrimes a b = isPrime (concatNum a b) && isPrime (concatNum b a)

concatenablePrimes :: Integral a => [a] -> [[a]]
concatenablePrimes xs = [[a, b, c, d, e] | a <- xs
                                         , b <- dropWhile (< a) xs
                                         , c <- dropWhile (< b) xs
                                         , d <- dropWhile (< c) xs
                                         , e <- dropWhile (< d) xs
                                         , checkPrimes a b
                                         , checkPrimes a c
                                         , checkPrimes a d
                                         , checkPrimes a e
                                         , checkPrimes b c
                                         , checkPrimes b d
                                         , checkPrimes b e
                                         , checkPrimes c d
                                         , checkPrimes c e
                                         , checkPrimes d e
                                         ]

solution60 :: IO ()
solution60 = do
    print $ minimum $ map sum (concatenablePrimes primes')
