-- works

module Problem55 where

import Common (digits, undigits)

isLychrel :: Integer -> Bool
isLychrel x = iterateUntilPalindrom x 0 where
    iterateUntilPalindrom num count
        | count == 50 = True
        | count > 0 && digs == revdigs = False
        | otherwise = iterateUntilPalindrom num' (count + 1)
        where digs = digits num
              revdigs = reverse digs
              num' = num + undigits revdigs

solution55 :: IO ()
solution55 = do
    print $ length $ filter isLychrel [1..10000]
