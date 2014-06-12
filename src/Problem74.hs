
module Problem74 where

import Common (digits, factorial)

facDigits :: Int -> Int
facDigits x = sum $ map factorial $ digits x

facChain :: Int -> [Int]
facChain = iterate facDigits

loopElements :: [Int]
loopElements = [1, 2, 145, 169, 871, 872, 1454, 40585, 45361, 45362, 363601]

chainLength :: Int -> Int
chainLength n = length chain + x where
    (chain, r:_) = break (`elem` loopElements) $ facChain n
    x = case r of
            169 -> 3
            871 -> 2
            872 -> 2
            1454 -> 3
            45361 -> 2
            45362 -> 2
            363601 -> 3
            _ -> 1

solution74 :: IO ()
solution74 = do
    print $ length $ filter (== 60) $ map chainLength [1..1000000]

