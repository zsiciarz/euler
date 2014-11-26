
module Problem43 where

import Data.List (permutations)
import Common (undigits)

pandigitalDigits :: [[Integer]]
pandigitalDigits = filter ((/= 0) . head) $ permutations [0..9]

solution43 :: IO Integer
solution43 = do
    return $ sum [undigits $ reverse ds | ds@(_:d2:d3:d4:d5:d6:d7:d8:d9:d10:_) <- pandigitalDigits
                                       , (100*d8 + 10*d9 + d10) `mod` 17 == 0
                                       , (100*d7 + 10*d8 + d9) `mod` 13 == 0
                                       , (100*d6 + 10*d7 + d8) `mod` 11 == 0
                                       , (100*d5 + 10*d6 + d7) `mod` 7 == 0
                                       , (100*d4 + 10*d5 + d6) `mod` 5 == 0
                                       , (100*d3 + 10*d4 + d5) `mod` 3 == 0
                                       , even (100*d2 + 10*d3 + d4)
                                       ]
