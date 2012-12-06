
module Problem102 where

import System.IO
import Data.List.Split (splitOn)


data Point = Point Double Double deriving Show

data Triangle = Triangle Point Point Point deriving Show

readTriangle :: String -> Triangle
readTriangle s = Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3) where
                 [x1, y1, x2, y2, x3, y3] = map read $ splitOn "," s

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

sides :: Triangle -> [Double]
sides (Triangle p1 p2 p3) = [distance p1 p2, distance p2 p3, distance p3 p1]

area :: Triangle -> Double
area triangle = sqrt(s * (s - a) * (s - b) * (s - c)) where
                [a, b, c] = sides triangle
                s = (a + b + c) / 2.0

triangleContains :: Triangle -> Point -> Bool
triangleContains triangle@(Triangle p1 p2 p3) p = 1e-6 > abs (a - a1 - a2 - a3) where
                                                  a = area triangle
                                                  a1 = area (Triangle p p2 p3)
                                                  a2 = area (Triangle p1 p p3)
                                                  a3 = area (Triangle p1 p2 p)

solution :: IO ()
solution = do
    h <- openFile "/home/zbyszek/triangles.txt" ReadMode
    contents <- hGetContents h
    print $ length $ filter (\t -> triangleContains t (Point 0 0)) $ map readTriangle $ lines contents

