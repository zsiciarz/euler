
module Problem71 where

solution :: IO ()
solution = do
    print $ maximum [a | b <- [999990..1000000]
                       , a <- [1..(b-1)]
                       , b * 3 - a * 7 == 1
                       ]
