
module Problem203 where

import Data.List (group, sort)
import Data.Numbers.Primes (primes)

pascal :: [[Integer]]
pascal = iterate (\r -> zipWith (+) ([0] ++ r) (r ++ [0])) [1]

distinctPascal :: Int -> [Integer]
distinctPascal n = map head $ group $ sort $ concat $ take n pascal

primeSquares :: [Integer]
primeSquares = map (^2) primes

isSquarefree :: Integer -> Bool
isSquarefree n = all (\s -> n `mod` s /= 0) $ takeWhile (<=n) $ primeSquares

solution :: IO ()
solution = do
    print $ sum $ filter isSquarefree $ distinctPascal 51
