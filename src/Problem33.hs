
module Problem33 where

import Data.Ratio

solution :: IO ()
solution = do
    print $ denominator $ product [(10*x + y) % (10*y + z) | x <- [1..9]
                                                           , y <- [1..9]
                                                           , z <- [1..9]
                                                           , x /= y
                                                           , 9*x*z + y*z == 10*x*y
                                                           ]
