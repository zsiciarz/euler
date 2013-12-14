
module Problem50 where

import Data.Numbers.Primes (primes, isPrime)
import Data.Ord (comparing)
import Data.List (maximumBy)

solution50 :: IO ()
solution50 = do
    print $ fst $ maximumBy (comparing snd) [ (s, length xs) | i <- [0..1000]
                                                             , j <- [i..1000]
                                                             , let xs = take j $ drop i primes
                                                             , let s = sum xs
                                                             , s < 1000000
                                                             , isPrime s
                                                             ]
