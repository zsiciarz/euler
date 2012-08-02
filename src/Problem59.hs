
module Problem59 where

import System.IO
import Data.List.Split (splitOn)

readInput :: String -> [Int]
readInput s = map read $ splitOn "," s

keys :: [String]
keys = [[a, b, c] | a <- ['a'..'z']
                  , b <- ['a'..'z']
                  , c <- ['a'..'z']
                  ]

solution :: IO ()
solution = do
    h <- openFile "D:\\cipher1.txt" ReadMode
    contents <- hGetContents h
    print $ keys
