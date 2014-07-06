
module Problem102 where

import System.IO (withFile, IOMode(..), hGetContents)
import Control.Applicative ((<$>))
import Data.List.Split (splitOn)


data Point = Point Double Double deriving Show

origin :: Point
origin = Point 0 0

data Triangle = Triangle Point Point Point deriving Show

instance Read Triangle where
    readsPrec _ s = [(Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3), "")] where
                 [x1, y1, x2, y2, x3, y3] = map read $ splitOn "," s

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

sides :: Triangle -> [Double]
sides (Triangle p1 p2 p3) = [distance p1 p2, distance p2 p3, distance p3 p1]

area :: Triangle -> Double
area triangle = sqrt(s * (s - a) * (s - b) * (s - c)) where
                [a, b, c] = sides triangle
                s = (a + b + c) / 2.0

triangleContains :: Point -> Triangle -> Bool
triangleContains p triangle@(Triangle p1 p2 p3) = 1e-6 > abs (a - a1 - a2 - a3) where
                                                  a = area triangle
                                                  a1 = area (Triangle p p2 p3)
                                                  a2 = area (Triangle p1 p p3)
                                                  a3 = area (Triangle p1 p2 p)

solution102 :: IO ()
solution102 = do
    withFile "data/triangles.txt" ReadMode $ \h -> do
        triangles <- (map read . lines) <$> hGetContents h
        print . length . filter (triangleContains origin) $ triangles
