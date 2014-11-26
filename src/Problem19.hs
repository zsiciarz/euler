
module Problem19 where

import Data.List (genericLength)

isLeapYear :: Int -> Bool
isLeapYear year = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)

daysInFebruary :: Int -> Int
daysInFebruary year = if isLeapYear year then 29 else 28

yearDays :: Int -> [Int]
yearDays year = [31, daysInFebruary year, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

solution19 :: IO Integer
solution19 = do
    return . genericLength . filter (\x -> x `mod` 7 == 0) . scanl (+) 0 . concatMap yearDays $ [1901..2000]
