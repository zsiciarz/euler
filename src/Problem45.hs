
module Problem45 where

import Figurate (hexagonal, isPentagonal)

solution45 :: IO Integer
solution45 = do
    return $ head [x | x <- map hexagonal [144..], isPentagonal x]
