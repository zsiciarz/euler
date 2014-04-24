
module Problem36 where

import Common (digits)

decToBin :: Int -> [Int]
decToBin x = reverse $ decToBin' x where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in b : decToBin' a

palindromic10 :: Int -> Bool
palindromic10 x =  ds == reverse ds where
    ds = digits x

palindromic2 :: Int -> Bool
palindromic2 x = b == reverse b where
    b = decToBin x

solution36 :: IO ()
solution36 = do
    print $ sum [ x | x <- [0..1000000], odd x && palindromic10 x && palindromic2 x]
