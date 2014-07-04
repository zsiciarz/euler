
module Problem99 where

import Prelude hiding (exponent)
import System.IO (withFile, IOMode(..), hGetContents)
import Data.List (maximumBy)
import Data.Ord (comparing)

maximumIndex ::  Ord a => [a] -> Int
maximumIndex = fst . maximumBy (comparing snd) . zip [1..]

processLine :: String -> Float
processLine line = exponent * logBase 10.0 base where
    base = read b
    exponent = read e
    (b, _:e) = break (==',') line

solution99 :: IO ()
solution99 = do
    withFile "data/base_exp.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        print . maximumIndex . map processLine . lines $ contents
