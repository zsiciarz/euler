
module Problem87 where

import qualified Data.IntSet as S
import Data.List (genericLength)
import Data.Numbers.Primes

limit :: Int
limit = 50000000

primePowers :: Int -> [Int]
primePowers power = takeWhile (<limit) $ map (^power) primes

sortAndUnique :: [Int] -> [Int]
sortAndUnique = S.toAscList . S.fromList

solution87 :: IO Integer
solution87 = do
    let powerSums = [a + b + c | a <- primePowers 2, b <- primePowers 3, c <- primePowers 4]
    return . genericLength . takeWhile (<limit) . sortAndUnique $ powerSums
