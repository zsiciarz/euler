module Main where

import System.Environment (getArgs)
import Solutions (runSolution)


main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> putStrLn "Usage: euler <PROBLEM NUMBER>"
        (problem:_) -> runSolution problem
