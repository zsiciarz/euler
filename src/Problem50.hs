
module Problem50 where

import Data.Numbers.Primes (primes, isPrime)
import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Vector.Unboxed as V

primes' :: V.Vector Int
primes' = V.fromList $ takeWhile (<1000000) primes

solution50 :: IO Integer
solution50 = do
    return $ fromIntegral $ fst $ maximumBy (comparing snd) [(s, j) | i <- [0..1000]
                                                            , j <- [i..1000]
                                                            , let s = V.sum $ V.slice i j primes'
                                                            , s < 1000000
                                                            , isPrime s
                                                            ]
