
module Problem58 where

import Data.Numbers.Primes (isPrime)

sideLength :: Int -> Int
sideLength n = 2 * n + 1

diagonalsCount :: Int -> Int
diagonalsCount n = 4 * n + 1

primesCount :: Int -> Int
primesCount n = counts !! n where
        counts = scanl primesCount' 0 [1..]
        primesCount' acc 0 = acc
        primesCount' acc n' = acc + (length $ filter isPrime diagonals) where
                diagonals = [s - 2 * n', s - 4 * n', s - 6 * n']
                s = (2 * n' + 1)^2

ratio :: Int -> Double
ratio n = fromIntegral (primesCount n) / fromIntegral (diagonalsCount n)

solution :: IO ()
solution = do
    print $ sideLength $ head $ dropWhile (\n -> ratio n > 0.1) [1..]

