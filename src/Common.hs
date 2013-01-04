
module Common where

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
