
module Problem87 where

import Data.List (group, sort)
import Data.Numbers.Primes

limit :: Integer
limit = 50000000

primePowers :: Integer -> [Integer]
primePowers power = takeWhile (<limit) $ map (^power) primes

sortAndUnique :: [Integer] -> [Integer]
sortAndUnique = map head . group . sort

solution87 :: IO ()
solution87 = do
    print $ length $ takeWhile (<limit) $ sortAndUnique [a + b + c | a <- primePowers 2
                                                                   , b <- primePowers 3
                                                                   , c <- primePowers 4
                                                                   ]
