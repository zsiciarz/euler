
module Problem19 where

isLeapYear :: Int -> Bool
isLeapYear year = mod year 4 == 0 && (mod year 100 /= 0 || mod year 400 == 0)

daysInFebruary :: Int -> Int
daysInFebruary year = if isLeapYear year then 29 else 28

yearDays :: Int -> [Int]
yearDays year = [31, daysInFebruary year, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

solution19 :: IO ()
solution19 = do
    print $ length $ filter (\x -> x `mod` 7 == 0) $ scanl (+) 0 $ concatMap yearDays [1901..2000]
