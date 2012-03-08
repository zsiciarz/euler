
module Problem2 where

fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)

solution :: IO ()
solution = do
    print $ sum [x | x <- takeWhile (<4000000) fib, even x]
