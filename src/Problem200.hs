-- brute force, too long

module Problem200 where

import Data.List (isInfixOf, sort)
import Data.Numbers.Primes (primes, isPrime)

import Common (digits, undigits)

primes' :: [Integer]
primes' = take 1000 primes

squbes :: [Integer]
squbes = [ s | p <- primes', q <- primes'
             , p /= q
             , let s = p^2 * q^3
             , let ds = digits s
             , [0, 0, 2] `isInfixOf` ds
             ]

isPrimeProof :: [Integer] -> Bool
isPrimeProof ds = all (not . isPrime) $ concat [
        [ undigits $ replaceDigit ds idx n | n <- [0..9], idx > 0 || odd n ]
        | (_, idx) <- zip ds [0..]
    ]

replaceDigit :: [Integer] -> Integer -> Integer -> [Integer]
replaceDigit digs index replacement = [if i == index then replacement else d | (d, i) <- zip digs [0..]]

solution200 :: IO ()
solution200 = do
    print $ head $ drop 199 $ filter (isPrimeProof . digits) $ sort squbes

