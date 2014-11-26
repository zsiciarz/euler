
module Problem62 where

import Control.Monad (forM)
import Control.Monad.State (State, get, put, evalState)
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Common (digits)

cubes :: [Integer]
cubes = [ x^3 | x <- [1..] ]

-- map from sorted digits to (lowest number, cubic permutation count)
type Register = M.Map [Integer] (Integer, Int)

countPermutations :: [Integer] -> State Register [(Integer, Int)]
countPermutations xs = forM xs $ \x -> do
    let ds = sort $ digits x
    register <- get
    let newRegister = M.insertWith (\(_, _) (x', c) -> (x', c + 1)) ds (x, 1) register
    put newRegister
    return $ fromJust $ M.lookup ds newRegister

solution62 :: IO Integer
solution62 = do
    let result = evalState (countPermutations cubes) M.empty
    return $ fst . head . filter ((== 5) . snd) $ result
