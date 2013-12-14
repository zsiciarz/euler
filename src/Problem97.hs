-- works, brute force

module Problem97 where

lastDigits :: Integer -> Int -> Integer
lastDigits x n = lastDigits' x n 0 where
    lastDigits' _ 0 _ = 0
    lastDigits' x' n' p = (m * 10^p) + (lastDigits' d (n'-1) (p+1)) where
            (d, m) = x' `quotRem` 10

solution97 :: IO ()
solution97 = do
    print $ lastDigits (28433 * 2^7830457 + 1) 10
