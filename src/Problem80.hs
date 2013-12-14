
module Problem80 where

import Common (digits)

solution80 :: IO ()
solution80 = do
    print $ floor $ frac * 10^100 where
        (_, frac) = properFraction $ sqrt 2.0
