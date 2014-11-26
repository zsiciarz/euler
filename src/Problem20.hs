
module Problem20 where

import Common (factorial, digits)

solution20 :: IO Integer
solution20 = do
    return $ (sum . digits . factorial) 100
