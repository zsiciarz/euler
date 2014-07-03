-- bad results after ~2h run time

module Problem204 where

primes' :: [Int]
import Data.Numbers.Primes (primeFactors, primes)

isGeneralisedHamming :: Int -> Int -> Bool
isGeneralisedHamming n x = not $ any (> n) (primeFactors x)

primes' = take 9 $ dropWhile (<= 5) primes

basicFilter :: Int -> Bool
basicFilter x = all (\n -> x `mod` n /= 0) primes'

solution204 :: IO ()
solution204 = do
    print $ length [x | x <- [1..10^9]
                      , basicFilter x
                      , isGeneralisedHamming 100 x
                      ]
