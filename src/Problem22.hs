{-# LANGUAGE OverloadedStrings #-}

module Problem22 where

import qualified Data.Text as T
import Data.Char (ord)
import System.IO (IOMode(..), withFile)
import Data.List (sort)
import Data.Text.IO (hGetContents)

charValue :: Char -> Int
charValue c = ord c - 64 -- (ord 'A') + 1

nameValue :: T.Text -> Int
nameValue = T.foldl' (\acc c -> acc + charValue c) 0 . T.filter (`elem` ['A'..'Z'])

findSolution :: T.Text -> Int
findSolution s = sum [i * nameValue name | (i, name) <- zip [1..] $ sort (T.splitOn "," s)]

solution22 :: IO Integer
solution22 = do
    withFile "data/names.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        return $ fromIntegral $ findSolution contents
