-- todo

module Problem23 where

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
abundants = filter isAbundant [1..]

sumsOfNotAbundants :: [Int]
sumsOfNotAbundants = [x + y | x <- [1..28123]
                         , y <- [1..(28123-x)]
                         , (not $ isAbundant x) || (not $ isAbundant y)
                         ]

solution :: IO ()
solution = do
    print $ take 100 $ sumsOfNotAbundants
