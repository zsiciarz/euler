module Main where

import Options.Applicative
import Solutions (SolutionOptions (..), runSolution)

solutionOptions :: Parser SolutionOptions
solutionOptions = SolutionOptions <$> option (long "problem" <> metavar "NUM")

main :: IO ()
main = execParser opts >>= runSolution
    where
        opts = info (helper <*> solutionOptions) fullDesc
