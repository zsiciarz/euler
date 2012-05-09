-- needs reworking

module Problem38 where

import Data.List (nub, inits, sort)

makeSequence :: Int -> [Int]
makeSequence n = map (n*) [1..5]

digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

concatenate :: [Int] -> String
concatenate xs = concat $ map show xs

isPandigital :: String -> Bool
isPandigital s = (length $ nub s) == 9

makeNumber :: Int -> String
makeNumber x = last $ takeWhile (\s -> length s < 10) $ map concatenate $ inits $ zipWith (*) (repeat x) [1..]

solution :: IO ()
solution = do
    print $ sort $ [x | x <- map makeNumber [1..999999]
               , isPandigital x
               ]
