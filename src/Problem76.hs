-- to do

module Problem76 where

target :: Int
target = 100

numbers :: [Int]
numbers = [1..99]

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0
ways (c:cs) = n where
                n = zipWith (+) (ways cs) (replicate c 0 ++ n)


solution :: IO ()
solution = do
    print $ ways numbers !! target
