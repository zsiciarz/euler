
module Problem49 where

import Data.List (sort)
import Data.Numbers.Primes
import Common (digits)

primes' :: [Int]
primes' = dropWhile (<1000) $ takeWhile (<10000) primes

checkTriple :: (Int, Int, Int) -> Bool
checkTriple (a, b, c) = aa == bb && aa == cc
                    where aa = sort $ digits a
                          bb = sort $ digits b
                          cc = sort $ digits c

solution49 :: IO ()
solution49 = do
    print $ [ (p, ps, pss) | p <- primes'
                           , s <- [1..9000]
                           , let ps = p + s
                           , let pss = p + s + s
                           , isPrime ps
                           , isPrime pss
                           , pss < 10000
                           , checkTriple (p, ps, pss)
                           ]
