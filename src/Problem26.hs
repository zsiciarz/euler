
module Problem26 where

import Data.Function (on)
import Data.List (elemIndex, maximumBy)

recurringCycle :: Int -> Int
recurringCycle d = remainders d 10 []

remainders :: Int -> Int -> [Int] -> Int
remainders _ 0 _  = 0
remainders d r rs = let r' = r `mod` d
                    in case elemIndex r' rs of
                        Just i  -> i + 1
                        Nothing -> remainders d (10*r') (r':rs)

solution26 :: IO ()
solution26 = do
    print $ fst $ maximumBy (compare `on` snd) [(n,recurringCycle n) | n <- [1..999]]
