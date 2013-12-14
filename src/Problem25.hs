
module Problem25 where

fib :: [Int]
fib = 1 : 1 : zipWith (+) fib (tail fib)

solution25 :: IO ()
solution25 = do
    print $ length $ takeWhile (<1000) ( map (length . show) fib )
