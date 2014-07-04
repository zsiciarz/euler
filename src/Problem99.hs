
module Problem99 where

import System.IO (withFile, IOMode(..), hGetContents)
import Data.List (maximumBy)
import Data.Ord (comparing)

maximumIndex ::  Ord a => [a] -> Int
maximumIndex = fst . maximumBy (comparing snd) . zip [1..]

processLine :: String -> Float
processLine s = e' * logBase 10.0 b' where
                b' = read b
                e' = read e
                (b, _:e) = break (==',') s

solution99 :: IO ()
solution99 = do
    withFile "data/base_exp.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        print . maximumIndex . map processLine . lines $ contents
