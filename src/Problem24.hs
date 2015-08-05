
module Problem24 where

import Data.List (delete)
import Common (undigits, factorial)

findPermutation :: [Int] -> Int -> [Int]
findPermutation [] _ = []
findPermutation xs i = (xs !! q) : findPermutation xs' r where
    (q, r) = i `quotRem` factorial (length xs - 1)
    xs' = delete (xs !! q) xs


solution24 :: IO Integer
solution24 = do
    return $ fromIntegral $ undigits $ reverse $ findPermutation [0..9] 999999
