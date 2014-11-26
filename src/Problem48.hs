
module Problem48 where

solution48 :: IO Integer
solution48 = do
    return $ sum [x^x | x <- [1..1000]] `mod` 10^10
