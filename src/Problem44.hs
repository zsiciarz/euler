-- works, but guessing the 5000

module Problem44 where

import qualified Data.Set as S

pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `div` 2

pents :: [Int]
pents = map pentagonal [1..5000]

pentsS :: S.Set Int
pentsS = S.fromList pents

isPentagonal :: Int -> Bool
isPentagonal n = n `S.member` pentsS

solution :: IO ()
solution = do
    print $ head [ d | x <- pents
                     , y <- pents
                     , let s = x + y
                     , let d = x - y
                     , x > y
                     , isPentagonal s
                     , isPentagonal d
                     ]
