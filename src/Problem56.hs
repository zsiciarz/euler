
module Problem56 where

import Common (digits)

solution56 :: IO Integer
solution56 = do
    return $ maximum $ map (sum . digits) [a^b | a <- [1..99], b <- [1..99]]
