
module Problem36 where

import Common (digits, digitsBase)

palindromic10 :: Int -> Bool
palindromic10 x =  ds == reverse ds where
    ds = digits x

palindromic2 :: Int -> Bool
palindromic2 x = ds == reverse ds where
    ds = digitsBase 2 x

solution36 :: IO ()
solution36 = do
    print $ sum [ x | x <- [0..1000000], odd x && palindromic10 x && palindromic2 x]
