module Main where

import System.Environment (getArgs)
import Solutions (runSolution)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: euler <PROBLEM NUMBER>"
        _  -> do
            let problem = args !! 0
            runSolution problem
