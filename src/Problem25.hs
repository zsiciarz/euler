
module Problem25 where

import Data.List (genericLength)
import Common (fibs)

solution25 :: IO Integer
solution25 = do
    return $ genericLength $ takeWhile (<1000) (map (length . show) fibs)
