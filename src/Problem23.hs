-- works, runs in 22 seconds

module Problem23 where

limit :: Int
limit = 28123

divisors :: Int -> [Int]
divisors x = divisors' 2 [1] x
    where divisors' d divs n
            | d^2 > n      = divs
            | (mod n d)==0 = if (div n d) == d
                             then divisors' (d+1) (d : divs) n
                             else divisors' (d+1) (d : (div n d) : divs) n
            | otherwise    = divisors' (d+1) divs n

isAbundant :: Int -> Bool
isAbundant x = x < sum (divisors x)

abundants :: [Int]
abundants = filter isAbundant [1..limit]

isSum :: Int -> Bool
isSum x = any isAbundant $ rests x where
          rests n = map (n-) $ takeWhile (<= n `div` 2) abundants

solution23 :: IO Integer
solution23 = do
    return $ fromIntegral $ sum $ filter (not . isSum) [1..limit]
