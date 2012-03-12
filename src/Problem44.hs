-- does not work

module Problem44 where

pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `div` 2

pents :: [Int]
pents = map pentagonal [1..]

isNatural :: Double -> Bool
isNatural x = (snd $ properFraction x) == 0

isPentagonal :: Int -> Bool
isPentagonal n = isNatural $ (sqrt(24.0 * (fromIntegral n) + 1.0) + 1.0) / 6.0

solution :: IO ()
solution = do
    print $ take 20 [ (x,y) | x <- pents
                     , y <- pents
                     , isPentagonal $ x + y
                     , isPentagonal $ x - y
                     ]
