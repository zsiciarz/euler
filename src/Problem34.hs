
module Problem34 where

import qualified Data.Vector.Unboxed as V
import Common (digits)

factorials :: V.Vector Int
factorials = V.fromList [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]

facSum :: Int -> Int
facSum = sum . map (factorials V.!) . digits

solution34 :: IO Integer
solution34 = do
    return $ fromIntegral $ sum [ x | x <- [3..2540160], x == facSum x]
