
module Problem71 where

solution71 :: IO ()
solution71 = do
    print $ maximum [a | b <- [999990..1000000], a <- [1..(b - 1)], b * 3 - a * 7 == 1]
