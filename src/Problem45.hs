-- works, ok

module Problem45 where

import Figurate (hexagonal, isPentagonal)

solution :: IO ()
solution = do
    print $ head [ x | x <-  map hexagonal [144..]
                     , x > 40755
                     , isPentagonal x
                     ]
