
module Problem206 where

digits :: Integer -> [Integer]
digits 0 = []
digits x = (x `mod` 10) : digits (x `div` 10)

maxNumber :: Integer
maxNumber = floor $ sqrt 1929394959697989900

correctDigits :: Integer -> Bool
correctDigits n = digs !! 18 == 1 && digs !! 16 == 2 && digs !! 14 == 3 &&
                  digs !! 12 == 4 && digs !! 10 == 5 && digs !! 8 == 6 &&
                  digs !! 6 == 7 && digs !! 4 == 8 -- no need to check for 9
                  where
                    digs = digits n

-- last digits have to be: ...30 or ...70
validNumbers :: [Integer]
validNumbers = scanl (+) 1000000030 $ cycle [40, 60]

solution :: IO ()
solution = do
    print $ head $ filter (\n -> correctDigits $ n^2) $ validNumbers

