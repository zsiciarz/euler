
module Problem35 where

import Data.Numbers.Primes
import Data.List (genericLength, inits, tails)

import Common (digits, undigits)

rotations :: [a] -> [[a]]
rotations l = drop 1 $ map (\(x, y) -> y ++ x) (inits l `zip` tails l)

isCircular :: Int -> Bool
isCircular = all isPrime . map undigits . rotations . digits

solution35 :: IO Integer
solution35 = do
    return $ genericLength [x | x <- takeWhile (<1000000) primes, isCircular x]
