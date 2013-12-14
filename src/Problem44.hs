-- works, but guessing the 5000

module Problem44 where

import qualified Data.Set as S
import Figurate (pentagonals)

pents :: [Int]
pents = take 5000 pentagonals

pentsS :: S.Set Int
pentsS = S.fromList $ pents

isPentagonal :: Int -> Bool
isPentagonal n = n `S.member` pentsS

solution44 :: IO ()
solution44 = do
    print $ head [ d | x <- pents
                     , y <- pents
                     , let s = x + y
                     , let d = x - y
                     , x > y
                     , isPentagonal s
                     , isPentagonal d
                     ]
