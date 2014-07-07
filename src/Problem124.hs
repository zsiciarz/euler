
module Problem124 where

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Numbers.Primes (primeFactors)
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as V

rads :: V.Vector Int
rads = V.map rad' $ V.enumFromTo 1 100000 where
    rad' = S.foldr' (*) 1 . S.fromList . primeFactors

rad :: Int -> Int
rad n = rads V.! (n-1)

compareByRad :: Int -> Int -> Ordering
compareByRad x y = (compare `on` rad) x y <> x `compare` y

e :: Int -> Int
e n =  sortBy compareByRad [1..100000] !! (n-1)

solution124 :: IO ()
solution124 = do
    print $ e 10000
