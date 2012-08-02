-- works

module Problem55 where

digits :: Integer -> [Integer]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

undigits :: [Integer] -> Integer
undigits ds = sum $ zipWith (\a b -> a * 10^b) ds [0,1..]

isLychrel :: Integer -> Bool
isLychrel x = iterateUntilPalindrom x 0 where
    iterateUntilPalindrom num count
        | count == 50 = True
        | count > 0 && digs == revdigs = False
        | otherwise = iterateUntilPalindrom num' (count + 1)
        where digs = digits num
              revdigs = reverse digs
              num' = num + undigits revdigs

solution :: IO ()
solution = do
    print $ length $ filter isLychrel [1..10000]
