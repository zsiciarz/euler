-- ok, works

module Problem99 where

import System.IO
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

processLine :: String -> Float
processLine s = e' * logBase 10.0 b' where
                b' = read b::Float
                e' = read e::Float
                b:e:_ = splitOn "," s

solution :: IO ()
solution = do
    h <- openFile "D:\\base_exp.txt" ReadMode
    contents <- hGetContents h
    let numbers = zip [1..] $ map processLine $ lines contents
    let m = maximumBy (comparing snd) numbers
    print $ fst m
