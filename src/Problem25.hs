
module Problem25 where

fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)

solution :: IO ()
solution = do
    print $ length $ takeWhile (<1000) ( map (length . show) fib )
