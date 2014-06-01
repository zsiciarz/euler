
module Problem61 where

import Data.List (permutations)
import qualified Data.Set as S
import Figurate

takeFourDigits :: Integral a => [a] -> [a]
takeFourDigits = takeWhile (< 10000) . dropWhile (< 1000)

allFigurates :: Integral a => [[a]]
allFigurates = map takeFourDigits [triangulars, squares, pentagonals, hexagonals, heptagonals, octagonals]

allFigurates' :: Integral a => [a]
allFigurates' = concat allFigurates

allDifferent :: Integral a => [a] -> Bool
allDifferent xs = length xs == S.size (S.fromList xs) && and ps where
    ps = zipWith elem xs allFigurates

isCyclic :: Integral a => a -> a -> Bool
isCyclic x y = x `mod` 100 == y `div` 100

findSets :: Integral a => [a] -> [[a]]
findSets figurates = concatMap permutations [[a, b, c, d, e, f] | a <- figurates
                                                                , b <- filter (a `isCyclic`) figurates
                                                                , c <- filter (b `isCyclic`) figurates
                                                                , d <- filter (c `isCyclic`) figurates
                                                                , e <- filter (d `isCyclic`) figurates
                                                                , f <- filter (e `isCyclic`) figurates
                                                                , f `isCyclic` a
                                                                ]

solution61 :: IO ()
solution61 = do
    print $ sum . head $ filter allDifferent $ findSets allFigurates'
