
module Problem80 where

import Common (digits)

solution :: IO ()
solution = do
    print $ floor $ frac * 10^100 where
        (_, frac) = properFraction $ sqrt 2.0
