
module Problem64 where

import Data.List (genericLength)

findExpansionPeriod :: Int -> [Int]
findExpansionPeriod s = findExpansion' 0 1 a0 where
    a0 = floor . sqrt . fromIntegral $ s
    findExpansion' m d a
        | s == a0 * a0 = []
        | a == 2 * a0 = []
        | otherwise = a' : findExpansion' m' d' a' where
            m' = d * a - m
            d' = (s - m'^2) `div` d
            a' = (a0 + m') `div` d'

solution64 :: IO Integer
solution64 = do
    return $ genericLength $ filter (odd . length . findExpansionPeriod) [2..10000]
