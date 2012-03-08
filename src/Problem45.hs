
module Problem45 where

maxIndex :: Int
maxIndex = 10000

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2

triangles :: [Int]
triangles = map triangle [1..maxIndex]

pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `div` 2

pents :: [Int]
pents = map pentagonal [1..maxIndex]

hexagonal :: Int -> Int
hexagonal n = n * (2 * n - 1)

hexes :: [Int]
hexes = map hexagonal [1..maxIndex]

solution :: IO ()
solution = do
    print $ take 2 [ x | x <- triangles, y <- pents, z <- hexes, x == y, x == z, x > 1]
