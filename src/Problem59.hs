
module Problem59 where

import System.IO
import Data.Bits (xor)
import Data.Char (chr)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)

readInput :: String -> [Int]
readInput s = map read $ splitOn "," s

keys :: [[Int]]
keys = [[a, b, c] | a <- [97..122]
                  , b <- [97..122]
                  , c <- [97..122]
                  ]

decipher :: [Int] -> [Int] -> [Int]
decipher input key = zipWith xor input (concat $ repeat key)

hasCommonWords :: String -> Bool
hasCommonWords s = "the " `isInfixOf` s && "and " `isInfixOf` s

solution :: IO ()
solution = do
    h <- openFile "/home/zbyszek/cipher1.txt" ReadMode
    input <- fmap readInput $ hGetContents h
    print $ sum $ head $ filter (hasCommonWords . map chr) $ map (decipher input) keys
