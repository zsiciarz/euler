
module Problem87 where

import qualified Data.Set as S
import Data.Numbers.Primes

limit :: Integer
limit = 50000000

primePowers :: Integer -> [Integer]
primePowers power = takeWhile (<limit) $ map (^power) primes

sortAndUnique :: [Integer] -> [Integer]
sortAndUnique = S.toAscList . S.fromList

solution87 :: IO ()
solution87 = do
    print $ length $ takeWhile (<limit) $ sortAndUnique [a + b + c | a <- primePowers 2
                                                                   , b <- primePowers 3
                                                                   , c <- primePowers 4
                                                                   ]
