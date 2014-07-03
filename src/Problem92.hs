-- works, brute force

module Problem92 where

import Common (digits)

chain :: Int -> Int
chain 1 = 1
chain 89 = 89
chain n = chain . sum . map (^2) . digits $ n

solution92 :: IO ()
solution92 = do
    print $ length $ filter (\s -> chain s == 89) [1..10000000]
