
module Problem89 where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.IO (IOMode(..), withFile, hGetContents)

replace :: String -> String -> String -> String
replace search replacement = intercalate replacement . splitOn search

solution89 :: IO ()
solution89 = do
    withFile "data/roman.txt" ReadMode $ \h -> do
        contents <- hGetContents h
        let newLength = length $ replace "IIII" "IV"
                            $ replace "VIIII" "IX"
                            $ replace "XXXX" "XL"
                            $ replace "LXXXX" "XC"
                            $ replace "CCCC" "CD"
                            $ replace "DCCCC" "CM" contents
        print $ length contents - newLength
