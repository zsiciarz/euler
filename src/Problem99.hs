-- ok, works

module Problem99 where

import System.IO (withFile, IOMode(..), hGetContents)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

processLine :: String -> Float
processLine s = e' * logBase 10.0 b' where
                b' = read b::Float
                e' = read e::Float
                b:e:_ = splitOn "," s

solution99 :: IO ()
solution99 = do
    withFile "data/base_exp.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        let numbers = zip [1..] $ map processLine $ lines contents
        let m = maximumBy (comparing snd) numbers
        print $ fst m
