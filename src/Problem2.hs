
module Problem2 where

fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)

solution2 :: IO ()
solution2 = do
    print $ sum [x | x <- takeWhile (<4000000) fib, even x]
