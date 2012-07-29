-- works

module Problem38 where

import Data.List ( inits, sort)
import Common (digits)

concatenate :: [Int] -> String
concatenate xs = concat $ map show xs

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

makeNumber :: Int -> String
makeNumber x = last $ takeWhile (\s -> length s < 10) $ map concatenate $ inits $ zipWith (*) (repeat x) [1..]

solution :: IO ()
solution = do
    print $ last . sort $ [x | x <- map makeNumber [1..999999]
                             , isPandigital x
                             ]
