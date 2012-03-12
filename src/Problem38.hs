-- needs reworking

module Problem38 where

import Data.List (nub)

makeSequence :: Int -> [Int]
makeSequence n = map (n*) [1..5]

concatenate :: [Int] -> String
concatenate xs = concat $ map show xs

isPandigital :: String -> Bool
isPandigital s = (length $ nub s) == 9

solution :: IO ()
solution = do
    print $ [x | x <- map (concatenate . makeSequence) [1..10]
               , length x == 9
               , isPandigital x
               ]
