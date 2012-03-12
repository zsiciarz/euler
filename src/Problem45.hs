-- works, ok

module Problem45 where

hexagonal :: Int -> Int
hexagonal n = n * (2 * n - 1)

isNatural :: Double -> Bool
isNatural x = (snd $ properFraction x) == 0

isPentagonal :: Int -> Bool
isPentagonal n = isNatural $ (sqrt(24.0 * (fromIntegral n) + 1.0) + 1.0) / 6.0

solution :: IO ()
solution = do
    print $ head [ x | x <-  map hexagonal [144..]
                     , x > 40755
                     , isPentagonal x
                     ]
