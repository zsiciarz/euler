
module Problem97 where

import Common (digits, undigits)

solution97 :: IO Integer
solution97 = do
    return . undigits . take 10 . digits $ 28433 * 2^7830457 + 1
