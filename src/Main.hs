module Main where

import System.Environment (getArgs)
import Solutions (runSolution)


main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then putStrLn "Usage: euler <PROBLEM NUMBER>"
        else do
            let problem = args !! 0
            runSolution problem
