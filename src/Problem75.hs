
module Problem75 where

import Data.List (genericLength, group, sort)

maxLength :: Int
maxLength = 1500000

mlimit :: Int
mlimit = ceiling $ sqrt (fromIntegral maxLength)

lengths :: [Int]
lengths =  [l | m <- [2..mlimit]
              , n <- [1..m]
              , k <- [1..maxLength `div` 2]
              , odd (m + n)
              , gcd m n == 1
              , let l = 2 * k * m * (m + n)
              , l <= maxLength
              ]

solution75 :: IO Integer
solution75 = do
    return $ genericLength . filter (1==) . map length . group . sort $ lengths
