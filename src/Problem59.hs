
module Problem59 where

import System.IO
import Control.Applicative ((<$>))
import Data.Bits (xor)
import Data.Char (ord)
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

hasCommonWords :: [Int] -> Bool
hasCommonWords s = all (`isInfixOf` s) commonWords where
    commonWords = map (map ord) ["the ", "and "]

solution59 :: IO ()
solution59 = do
    withFile "data/cipher1.txt" ReadMode $ \h -> do
        input <- readInput <$> hGetContents h
        print $ (sum . head . filter hasCommonWords) $ map (decipher input) keys
