
module Problem22 where

import Data.Char (ord)
import System.IO
import Data.List (sort)
import Data.List.Split (splitOn)

charValue :: Char -> Int
charValue c = (ord c) - 64 -- (ord 'A') + 1

nameValue :: [Char] -> Int
nameValue name = sum $ map charValue $ filter (`elem` ['A'..'Z']) name

findSolution :: [Char] -> Int
findSolution s = sum [ i * (nameValue name) | (i, name) <- zip [1..] $ sort (splitOn "," s) ]

solution22 :: IO ()
solution22 = do
    h <- openFile "D:\\names.txt" ReadMode
    contents <- hGetContents h
    print $ findSolution contents
