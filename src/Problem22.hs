
module Problem22 where

import Data.Char (ord)
import System.IO (IOMode(..), withFile, hGetContents)
import Data.List (sort)
import Data.List.Split (splitOn)

charValue :: Char -> Int
charValue c = ord c - 64 -- (ord 'A') + 1

nameValue :: String -> Int
nameValue = sum . map charValue . filter (`elem` ['A'..'Z'])

findSolution :: String -> Int
findSolution s = sum [i * nameValue name | (i, name) <- zip [1..] $ sort (splitOn "," s)]

solution22 :: IO ()
solution22 = do
    withFile "data/names.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        print $ findSolution contents
