-- works, 18 minutes

module Problem145 where

import Data.List (genericLength)
import Common (digits, undigits)

reverseNumber :: Int -> Int
reverseNumber = undigits . reverse . digits

isReversible :: Int -> Bool
isReversible x = all odd $ digits s where
                 s = x + reverseNumber x

solution145 :: IO Integer
solution145 = do
    return $ genericLength [x | x <- [1..1000000000]
                        , x `mod` 10 /= 0
                        , isReversible x
                        ]
