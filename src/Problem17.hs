
module Problem17 where

lowNumbers :: [String]
lowNumbers = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
              "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

numberAsWord :: Int -> String
numberAsWord x
              | x < 20 = lowNumbers !! x
              | x < 100 = tens !! (d10 - 2) ++ numberAsWord m10
              | m100 == 0 = lowNumbers !! d100 ++ " hundred"
              | otherwise = lowNumbers !! d100 ++ " hundred and " ++ numberAsWord m100
              where (d10, m10) = divMod x 10
                    (d100, m100) = divMod x 100

numbers :: String
numbers = (concat $ map numberAsWord [1..999]) ++ "onethousand"

solution :: IO ()
solution = do
    print $ length $ filter (`elem` ['a'..'z']) numbers
