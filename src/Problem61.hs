
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

findMatchingNumbers :: Integral a => a -> [a] -> [a]
findMatchingNumbers n = filter (\x -> x `div` 100 == n `mod` 100)

findSets :: Integral a => [a] -> [[a]]
findSets figurates = concatMap permutations [[a, b, c, d, e, f] | a <- figurates
                                                                , b <- findMatchingNumbers a figurates
                                                                , c <- findMatchingNumbers b figurates
                                                                , d <- findMatchingNumbers c figurates
                                                                , e <- findMatchingNumbers d figurates
                                                                , f <- findMatchingNumbers e figurates
                                                                , f `mod` 100 == a `div` 100
                                                                ]

solution61 :: IO ()
solution61 = do
    print $ sum . head $ filter allDifferent $ findSets allFigurates'
