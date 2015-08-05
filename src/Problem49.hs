
module Problem49 where

import Data.List (sort)
import Data.Numbers.Primes
import Common (digits)

primes' :: [Int]
primes' = dropWhile (<1000) $ takeWhile (<10000) primes

checkPermutations :: [Int] -> Bool
checkPermutations xs = all (== x) xs' where
    (x:xs') = map (sort . digits) xs

solution49 :: IO Integer
solution49 = do
    let (p', ps', pss') = head [(p, ps, pss) | p <- primes'
                            , s <- [1..9000]
                            , let ps = p + s
                            , let pss = p + s + s
                            , isPrime ps
                            , isPrime pss
                            , pss < 10000
                            , checkPermutations [p, ps, pss]
                            , p /= 1487
                            ]
    return $ fromIntegral (p' * 100000000 + ps' * 10000 + pss')
