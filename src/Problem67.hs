-- works, ok

module Problem67 where

step :: [Int] -> [Int] -> [Int]
step [] [x] = [x]
step (x:xs) (y:yy:ys) = x + max y yy : step xs (yy:ys)

readInt :: String -> Int
readInt = read

solution67 :: IO ()
solution67 = do
    contents <- readFile "D:\\triangle.txt"
    let input = map (map readInt . words) $ lines contents
    print $ head $ foldr1 step input
