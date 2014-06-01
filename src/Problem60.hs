module Problem60 where

-- possibly works, too slow

import Data.Numbers.Primes (primes, isPrime)

primes' :: Integral a => [a]
primes' = takeWhile (< 10000) primes

concatNum :: Integral a => a -> a -> a
concatNum a b = a * 10 ^ p + b where
        p = floor $ 1.0 + logBase (10.0::Double) (fromIntegral b)

checkPrimes :: Integral a => a -> a -> Bool
checkPrimes a b = isPrime (concatNum a b) && isPrime (concatNum b a)

concatenablePrimes :: Integral a => [a] -> [[a]]
concatenablePrimes xs = [[a, b, c, d, e] | a <- xs
                                         , let bs = filter (checkPrimes a) $ dropWhile (<= a) xs
                                         , b <- bs
                                         , let cs = filter (checkPrimes b) $ dropWhile (<= b) bs
                                         , c <- cs
                                         , let ds = filter (checkPrimes c) $ dropWhile (<= c) cs
                                         , d <- ds
                                         , let es = filter (checkPrimes d) $ dropWhile (<= d) ds
                                         , e <- es
                                         ]

solution60 :: IO ()
solution60 = do
    print $ sum . head $ concatenablePrimes primes'
