{-# LANGUAGE OverloadedStrings #-}

module Problem42 where

import Data.Char (ord)
import System.IO (IOMode(..), withFile)
import qualified Data.Text as T
import Data.Text.IO (hGetContents)
import Data.List (genericLength)

import Figurate (isTriangular)

charValue :: Char -> Int
charValue c = (ord c) - (ord 'A') + 1

wordValue :: T.Text -> Int
wordValue word = T.foldl' (\acc ch -> acc + charValue ch) 0 $ T.filter (`elem` ['A'..'Z']) word

solution42 :: IO Integer
solution42 = do
    withFile "data/words.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        let words = T.splitOn "," contents
        return $ genericLength $ filter (isTriangular . wordValue) words
