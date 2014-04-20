
module Problem17 where

lowNumbers :: [Int]
lowNumbers = map length ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
              "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [Int]
tens = map length ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

letterCount :: Int -> Int
letterCount x
    | x < 20 = lowNumbers !! x
    | x < 100 = tens !! (d10 - 2) + letterCount m10
    | m100 == 0 = lowNumbers !! d100 + length "hundred"
    | otherwise = lowNumbers !! d100 + length "hundredand" + letterCount m100
    where (d10, m10) = divMod x 10
          (d100, m100) = divMod x 100

solution17 :: IO ()
solution17 = do
    print $ foldr (\x y -> letterCount x + y) (length "onethousand") [1..999]
