
module Problem48 where

solution48 :: IO ()
solution48 = do
    print $ sum [x^x | x <- [1..1000]] `mod` 10^10
