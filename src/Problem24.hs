
module Problem24 where

import Data.List (permutations, sort)
import Common (undigits)

solution24 :: IO Integer
solution24 = do
    let perms = sort $ permutations [0..9]
    return $ undigits $ reverse $ perms !! 999999
