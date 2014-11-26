
module Problem28 where

sumDiagonals :: Integer -> (Integer, Integer)
sumDiagonals 1 = (1, 1)
sumDiagonals level = (prevSum + 4 * prevValue + 10 * step, prevValue + 4 * step)
    where (prevSum, prevValue) = sumDiagonals (level - 1)
          step = 2 * (level - 1)

solution28 :: IO Integer
solution28 = do
    return $ fst (sumDiagonals 501)
