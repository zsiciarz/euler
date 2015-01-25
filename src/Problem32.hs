
module Problem32 where

import Data.List (permutations)
import qualified Data.IntSet as S
import qualified Data.Map as M
import Common (digits)

undigits :: M.Map [Int] Int
undigits = foldr (\x m -> M.insert (digits x) x m) M.empty [1..100000]

undigits' :: [Int] -> Int
undigits' ds = M.findWithDefault 0 ds undigits

products :: [Int]
products = [undigits' r | xs <- permutations [1..9]
           , i <- [1..5]
           , j <- [1..8-i]
           , let (p, rest) = splitAt i xs
           , let (q, r) = splitAt j rest
           , undigits' p * undigits' q == undigits' r
           ]

solution32 :: IO Integer
solution32 = do
    return $ fromIntegral $ S.foldr' (+) 0 $ S.fromList products
