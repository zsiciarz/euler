
module Problem30 where

digits :: Int -> [Int]
digits x = [ read [c]::Int | c <- show x ]

sumPowers :: Int -> Int -> Int
sumPowers pow = sum . map (^pow) . digits

solution :: IO ()
solution = do
    print $ sum [ x | x <- [2..1234567], x == sumPowers 5 x]
