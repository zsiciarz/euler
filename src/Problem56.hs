
module Problem56 where

import Data.Char

numbers :: [Int]
numbers = [ a^b | a <- [1..99], b <- [1..99] ]

digitalSum :: Int -> Int
digitalSum n = sum $ map digitToInt $ show n

solution56 :: IO ()
solution56 = do
    print $ maximum $ map digitalSum numbers
