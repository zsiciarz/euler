-- works

module Problem38 where

import Data.List ( inits, sort)

concatenate :: [Int] -> String
concatenate = concatMap show

isPandigital :: String -> Bool
isPandigital s = sort s == "123456789"

makeNumber :: Int -> String
makeNumber x = last $ takeWhile (\s -> length s < 10) $ map concatenate $ inits $ zipWith (*) (repeat x) [1..]

solution38 :: IO ()
solution38 = do
    print $ maximum [x | x <- map makeNumber [1..999999]
                       , isPandigital x
                       ]
