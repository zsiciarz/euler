
module Problem56 where

import Common (digits)

numbers :: [Integer]
numbers = [ a^b | a <- [1..99], b <- [1..99] ]

digitalSum :: Integer -> Integer
digitalSum n = sum $ digits n

solution56 :: IO ()
solution56 = do
    print $ maximum $ map digitalSum numbers
