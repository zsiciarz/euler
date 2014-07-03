
module Problem92 where

import Common (digits)
import Data.Array.Unboxed

limit :: Int
limit = 10000000

sumSquareDigits :: Int -> Int
sumSquareDigits = sum . map (^2) . digits

chainCache :: UArray Int Int
chainCache = listArray (1, 7 * 9^2) . map sumSquareDigits $ [1..7 * 9^2]

chain :: Int -> Int
chain 1 = 1
chain 89 = 89
chain n = chain $ chainCache ! n

solution92 :: IO ()
solution92 = do
    print . length . filter (\s -> (chain . sumSquareDigits) s == 89) $ [1..limit]
