
module Problem89 where

import System.IO
import Text.Regex

replace :: String -> String -> String -> String
replace search replacement source = subRegex (mkRegex search) source replacement

solution89 :: IO ()
solution89 = do
    h <- openFile "/home/zbyszek/roman.txt" ReadMode
    contents <- hGetContents h
    let newLength = length $ replace "IIII" "IV"
                           $ replace "VIIII" "IX"
                           $ replace "XXXX" "XL"
                           $ replace "LXXXX" "XC"
                           $ replace "CCCC" "CD"
                           $ replace "DCCCC" "CM" contents
    print $ length contents - newLength
