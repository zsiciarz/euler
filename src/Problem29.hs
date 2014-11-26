
module Problem29 where

import qualified Data.Set as S

solution29 :: IO Integer
solution29 = do
    return $ fromIntegral . S.size . S.fromList $ [a^b | a <- [2..100], b <- [2..100]]
