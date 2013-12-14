
module Problem32 where
-- works, 6 seconds

import Data.List (nub, permutations)
import Common (undigits)

undigits' :: [Int] -> Int
undigits' = undigits . reverse

products :: [Int]
products = [ undigits' r | xs <- permutations [1..9]
               , i <- [1..5]
               , j <- [1..8-i]
               , let (p, rest) = splitAt i xs
               , let (q, r) = splitAt j rest
               , (undigits' p) * (undigits' q) == (undigits' r)
               ]

solution32 :: IO ()
solution32 = do
    print $ sum $ nub products
