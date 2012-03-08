
module Problem34 where

facs :: [Int]
facs = scanl (*) 1 [1..]

fac :: Int -> Int
fac n = facs !! n

digits :: Int -> [Int]
digits x = [ read [c]::Int | c <- show x ]

facSum :: Int -> Int
facSum x = sum $ map fac (digits x)

solution :: IO ()
solution = do
    print $ sum [ x | x <- [3..2540160], x == facSum x]
