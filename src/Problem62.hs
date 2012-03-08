
module Problem62 where

import Data.List (nub, permutations)

cubes :: [Integer]
cubes = [ x^3 | x <- [1000..10000] ]

findOthers :: Integer -> [String]
findOthers n = nub $ filter (\s -> (read s::Integer) `elem` cubes && head s /= '0') (permutations (show n))

solution :: IO ()
solution = do
    print $ [ x | x <- map findOthers cubes, length x == 5 ]
