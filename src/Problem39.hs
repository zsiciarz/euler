
module Problem39 where

import Data.Function (on)
import Data.List (group, sort, maximumBy)

pythagoreanPerimeters :: [Int]
pythagoreanPerimeters = [x + y + z | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]

solution39 :: IO Integer
solution39 = do
    return $ fromIntegral $ head $ maximumBy (compare `on` length) $ group $ sort $ takeWhile (<=1000) pythagoreanPerimeters
