
module Problem42 where

import Data.Char
import System.IO
import Data.List.Split (splitOn)

import Figurate (isTriangular)

charValue :: Char -> Int
charValue c = (ord c) - (ord 'A') + 1

wordValue :: [Char] -> Int
wordValue word = sum $ map charValue $ filter (`elem` ['A'..'Z']) word

findSolution :: [Char] -> [[Char]]
findSolution s = [ word | word <- splitOn "," s, isTriangular $ wordValue word ]

solution :: IO ()
solution = do
        h <- openFile "D:\\words.txt" ReadMode
        contents <- hGetContents h
        print $ length $ findSolution contents
