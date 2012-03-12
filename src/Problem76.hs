-- to do

module Problem76 where

ways :: Int -> [Int]
ways 1 = [1]
ways n = concatMap (\s -> s : ways s) [1..(n-1)]

solution :: IO ()
solution = do
    print $ ways 5
