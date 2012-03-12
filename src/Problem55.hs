-- does not work

module Problem55 where

digits :: Int -> [Int]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

undigits :: [Int] -> Int
undigits revdigs = sum $ zipWith (\a b -> a * 10^b) revdigs [0,1..]

isPalindromic :: [Int] -> Bool
isPalindromic digs = digs == reverse digs

iterateUntilPalindrom :: Int -> Bool
iterateUntilPalindrom x = iterateUntilPalindrom' x 0 where
    iterateUntilPalindrom' num count
                                | count >= 50 = False
                                | digs == revdigs = True
                                | otherwise = iterateUntilPalindrom' num' (count + 1)
                                where digs = digits num
                                      revdigs = reverse digs
                                      num' = num + undigits revdigs

solution :: IO ()
solution = do
    print $ undigits $ reverse $ digits 349
    print $ iterateUntilPalindrom 196
