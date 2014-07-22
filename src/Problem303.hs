
module Problem303 where

correctDigits :: Int -> Bool
correctDigits 0 = True
correctDigits x = validDigit (x `mod` 10) && correctDigits (x `div` 10) where
        validDigit 0 = True
        validDigit 1 = True
        validDigit 2 = True
        validDigit _ = False

f :: Int -> Int
f 9 = 9 * 1358
f 99 = 99 * 11335578
f 999 = 999 * 111333555778
f 9999 = 9999 * 1111333355557778
f n = head [x | x <- [n, 2*n..], correctDigits x]

solution303 :: IO ()
solution303 = do
    let results = map (\n -> f n `div` n) [1..10000]
    -- print $ results
    print $ sum results
