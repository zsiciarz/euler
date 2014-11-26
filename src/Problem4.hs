
module Problem4 where

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

solution4 :: IO Integer
solution4 = do
    return $ fromIntegral $ maximum [x * y | x <- [100..999]
                           , y <- [100..999]
                           , isPalindrome $ x * y
                           ]
