
module Figurate
(   triangular, triangulars, isTriangular
,   square, squares, isSquare
,   pentagonal, pentagonals, isPentagonal
,   hexagonal, hexagonals, isHexagonal
,   heptagonal, heptagonals
,   octagonal, octagonals
) where

isNatural :: Double -> Bool
isNatural x = (snd $ properFraction x) == 0


triangular :: Integral a => a -> a
triangular n = n * (n + 1) `div` 2

triangulars :: Integral a => [a]
triangulars = map triangular [1..]

isTriangular :: Integral a => a -> Bool
isTriangular n = isSquare $ 8 * n + 1

square :: Integral a => a -> a
square n = n^2

squares :: Integral a => [a]
squares = map square [1..]

isSquare :: Integral a => a -> Bool
isSquare = isNatural . sqrt . fromIntegral

pentagonal :: Integral a => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

pentagonals :: Integral a => [a]
pentagonals = map pentagonal [1..]

isPentagonal :: Integral a => a -> Bool
isPentagonal n = isNatural $ (sqrt(24.0 * (fromIntegral n) + 1.0) + 1.0) / 6.0

hexagonal :: Integral a => a -> a
hexagonal n = n * (2 * n - 1)

hexagonals :: Integral a => [a]
hexagonals = map hexagonal [1..]

isHexagonal :: Integral a => a -> Bool
isHexagonal n = isNatural $ (sqrt(8.0 * (fromIntegral n) + 1.0) + 1.0) / 4.0

heptagonal :: Integral a => a -> a
heptagonal n = n * (5 * n - 3) `div` 2

heptagonals :: Integral a => [a]
heptagonals = map heptagonal [1..]

octagonal :: Integral a => a -> a
octagonal n = n * (3 * n - 2)

octagonals :: Integral a => [a]
octagonals = map octagonal [1..]
