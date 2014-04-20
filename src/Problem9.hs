
module Problem9 where

type Sides = (Int, Int, Int)

sides :: [Sides]
sides = [(a, b, (1000 - (a + b))) | a <- [1..500]
                                  , b <- [a + 1..1000]
                                  , b <= 500
                                  ]

isPythagorean :: Sides -> Bool
isPythagorean (a, b, c) = (a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (c^2 + b^2 == a^2)

prodSides :: Sides -> Int
prodSides (a, b, c) = a * b * c

solution9 :: IO ()
solution9 = do
    print $ prodSides $ head $ filter isPythagorean sides
