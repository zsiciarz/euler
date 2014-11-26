
module Problem12 where

import Figurate (triangulars)

import Common (numDivisors)

solution12 :: IO Integer
solution12 = do
    return $ head $ dropWhile (\x -> numDivisors x < 500) triangulars
