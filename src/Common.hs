
module Common where

digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

undigits :: [Int] -> Int
undigits ds = sum $ zipWith (\a b -> a * 10^b) ds [0,1..]

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

factorial :: Int -> Integer
factorial n = factorials !! n

fibs :: [Integer]
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

fib :: Int -> Integer
fib n = fibs !! n
