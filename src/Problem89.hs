{-# LANGUAGE OverloadedStrings #-}

module Problem89 where

import qualified Data.Text as T
import System.IO (IOMode(..), withFile)
import Data.Text.IO (hGetContents)

replace :: T.Text -> T.Text -> T.Text -> T.Text
replace search replacement = T.intercalate replacement . T.splitOn search

solution89 :: IO Integer
solution89 = do
    withFile "data/roman.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        let newLength = T.length $ replace "IIII" "IV"
                            $ replace "VIIII" "IX"
                            $ replace "XXXX" "XL"
                            $ replace "LXXXX" "XC"
                            $ replace "CCCC" "CD"
                            $ replace "DCCCC" "CM" contents
        return $ fromIntegral $ T.length contents - newLength
