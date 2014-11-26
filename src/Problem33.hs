
module Problem33 where

import Data.Ratio

solution33 :: IO Integer
solution33 = do
    return $ denominator $ product [(10*x + y) % (10*y + z) | x <- [1..9]
                                                           , y <- [1..9]
                                                           , z <- [1..9]
                                                           , x /= y
                                                           , 9*x*z + y*z == 10*x*y
                                                           ]
