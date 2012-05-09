
module Problem31 where

target :: Int
target = 200

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0
ways (c:cs) = n where
                n = zipWith (+) (ways cs) (replicate c 0 ++ n)


solution :: IO ()
solution = do
    print $ ways coins !! target
