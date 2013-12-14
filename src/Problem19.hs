
module Problem19 where

isLeapYear :: Int -> Bool
isLeapYear year = mod year 4 == 0 && (mod year 100 /= 0 || mod year 400 == 0)

daysInFebruary :: Int -> Int
daysInFebruary year = if isLeapYear year then 29 else 28

yearDays :: Int -> [Int]
yearDays year = [31, daysInFebruary year, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

firstSundays :: Int -> Int -> ([Bool], Int)
firstSundays year prevLastDay = ([d `mod` 7 == 0 | d <- take 12 accumulated], lastDay)
            where accumulated = scanl (+) prevLastDay (yearDays year)
                  lastDay = last accumulated `mod` 7

countFirstSundays :: Int -> Int -> Int
countFirstSundays 2001 _ = 0
countFirstSundays year prevLastDay = (length sundays) + countFirstSundays (year + 1) lastDay
                    where (sundays, lastDay) = firstSundays year prevLastDay

solution19 :: IO ()
solution19 = undefined
