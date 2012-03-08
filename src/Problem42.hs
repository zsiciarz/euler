
module Problem42 where

import Data.Char
import System.IO
import Data.List.Split (splitOn)

charValue :: Char -> Int
charValue c = (ord c) - (ord 'A') + 1

wordValue :: [Char] -> Int
wordValue word = sum $ map charValue $ filter (`elem` ['A'..'Z']) word

maxNum :: Int
maxNum = 30 * (charValue 'Z')

triangleNumbers :: [Int]
triangleNumbers = takeWhile (<maxNum) [ sum [1..x] | x <- [1..] ]

isTriangleWord :: [Char] -> Bool
isTriangleWord word =  wordValue word `elem` triangleNumbers

findSolution :: [Char] -> [[Char]]
findSolution s = [ word | word <- splitOn "," s, isTriangleWord word ]

solution :: IO ()
solution = do
        h <- openFile "words.txt" ReadMode
        contents <- hGetContents h
        print $ length $ findSolution contents
