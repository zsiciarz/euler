
module Problem12 where

import Figurate (triangulars)

import Common (numDivisors)

solution12 :: IO ()
solution12 = do
    print $ head $ dropWhile (\x -> numDivisors x < 500) triangulars
