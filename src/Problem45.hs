
module Problem45 where

import Figurate (hexagonal, isPentagonal)

solution45 :: IO ()
solution45 = do
    print $ head [x | x <- map hexagonal [144..], isPentagonal x]
