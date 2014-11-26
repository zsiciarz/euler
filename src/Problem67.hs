
module Problem67 where

step :: [Int] -> [Int] -> [Int]
step [] [x] = [x]
step (x:xs) (y:yy:ys) = x + max y yy : step xs (yy:ys)
step _ _ = []

readInt :: String -> Int
readInt = read

solution67 :: IO Integer
solution67 = do
    contents <- readFile "data/triangle.txt"
    let input = map (map readInt . words) $ lines contents
    return $ fromIntegral $ head $ foldr1 step input
