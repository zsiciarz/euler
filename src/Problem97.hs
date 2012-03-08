
module Problem97 where

nmp :: Integer
nmp = 28433 * 2^7830457 + 1

lastElems :: String -> Int -> String
lastElems s n = reverse $ lastElems' s n
    where lastElems' _ 0 = []
          lastElems' s' n' = (last s') : lastElems' (init s') (n'-1)

solution :: IO ()
solution = do
    print $ lastElems (show nmp) 10
