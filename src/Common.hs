
module Common where

import Data.List (group)
import Data.Numbers.Primes (primeFactors)

digits :: Integral a => a -> [a]
digits 0 = []
digits x = r : digits q where
           (q, r) = x `quotRem` 10

undigits :: Integral a => [a] -> a
undigits ds = sum $ zipWith (\a b -> a * 10^b) ds [0,1..]

factorials :: Integral a => [a]
factorials = scanl (*) 1 [1..]

factorial :: Integral a => Int -> a
factorial n = factorials !! n

fibs :: Integral a => [a]
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

fib :: Integral a => Int -> a
fib n = fibs !! n

--primePowers :: Integral a => a -> [(a, a)]
primePowers n = [(head x, length x) | x <- group $ primeFactors n]

divisors n = map product $ sequence [take (k+1) $ iterate (p*) 1 | (p,k) <- primePowers n]

numDivisors n = product [k+1 | (_,k) <- primePowers n]
