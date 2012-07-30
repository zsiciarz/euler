-- works, 6 seconds

module Problem32 where

import Data.List (nub, permutations)
import Common (undigits)

undigits' :: [Int] -> Int
undigits' = undigits . reverse

products :: [Int]
products = [ r | xs <- permutations [1..9]
               , i <- [1..5]
               , j <- [1..8-i]
               , let p = undigits' $ take i xs
               , let rest = drop i xs
               , let q = undigits' $ take j rest
               , let r = undigits' $ drop j rest
               , p * q == r
               ]

solution :: IO ()
solution = do
    print $ sum $ nub products
