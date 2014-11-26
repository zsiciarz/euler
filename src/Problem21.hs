
module Problem21 where

import qualified Data.Map as M

import Common (divisors)

sumDivisors :: Integral a => a -> a
sumDivisors x = sum . filter (/= x) $ divisors x

amicablePairs :: Integral a => a -> M.Map a a
amicablePairs upto = M.filterWithKey (\k v -> k /= v && M.lookup v divisorsMap == Just k) divisorsMap where
    divisorsMap = foldr (\x mp -> M.insert x (sumDivisors x) mp) M.empty [2..upto]

solution21 :: IO Integer
solution21 = do
    return $ sum . M.keys . amicablePairs $ 10000
