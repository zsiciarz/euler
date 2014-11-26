
module Problem71 where

solution71 :: IO Integer
solution71 = do
    return $ maximum [a | b <- [999990..1000000], a <- [1..(b - 1)], b * 3 - a * 7 == 1]
