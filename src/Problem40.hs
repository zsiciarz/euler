
module Problem40 where

import Common (digits)

-- brute force approach
fraction :: [Int]
fraction = concatMap (reverse . digits) [1..]

solution40 :: IO Integer
solution40 = do
    return $ fromIntegral $ product [fraction !! i | i <- [0, 9, 99, 999, 9999, 99999, 999999]]
