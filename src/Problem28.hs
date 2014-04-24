
module Problem28 where

sumDiagonals :: Integer -> (Integer, Integer)
sumDiagonals 1 = (1, 1)
sumDiagonals level = (prevSum + prevValue + step
                              + prevValue + 2 * step
                              + prevValue + 3 * step
                              + prevValue + 4 * step, prevValue + 4 * step)
    where (prevSum, prevValue) = sumDiagonals (level - 1)
          step = 2 * (level - 1)

solution28 :: IO ()
solution28 = do
    print $ fst (sumDiagonals 501)
