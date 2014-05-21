
module Problem56 where

import Common (digits)

solution56 :: IO ()
solution56 = do
    print $ maximum $ map (sum . digits) [a^b | a <- [1..99], b <- [1..99]]
