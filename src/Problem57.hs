-- works, 10 seconds

module Problem57 where

import Data.Function (on)
import Data.List (genericLength)
import Data.Ratio
import qualified Data.Vector as V

import Common (digits)

p :: V.Vector Integer -> Int -> Int -> Ratio Integer
p a m n
    | m == n = 1 % (a V.! m)
    | otherwise = 1 / ((a V.! m) % 1 + p a (m+1) n)

convergent :: V.Vector Integer -> Int -> Ratio Integer
convergent a n = V.head a % 1 + p a 1 n

fracSqrt2 :: V.Vector Integer
fracSqrt2 = V.cons 1 $ V.replicate 1000 2

checkDigits :: Ratio Integer -> Bool
checkDigits r = ((>) `on` length . digits) n d where
                n = numerator r
                d = denominator r

solution57 :: IO Integer
solution57 = do
    return $ genericLength [r | n <- [1..1000]
                      , let r = convergent fracSqrt2 n
                      , checkDigits r
                      ]

