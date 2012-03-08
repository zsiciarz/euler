
module Problem36 where

decToBin :: Int -> [Int]
decToBin x = reverse $ decToBin' x
        where
        decToBin' 0 = []
        decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

palindromic10 :: Int -> Bool
palindromic10 x = show x == reverse (show x)

palindromic2 :: Int -> Bool
palindromic2 x = b == reverse b where b = decToBin x

solution :: IO ()
solution = do
    print $ sum [ x | x <- [1..1000000], odd x && palindromic10 x && palindromic2 x]
