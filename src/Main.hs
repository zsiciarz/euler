module Main where

import Options.Applicative
import Solutions (SolutionOptions (..), runSolution)

solutionOptions :: Parser SolutionOptions
solutionOptions = SolutionOptions <$> strOption (long "problem" <> metavar "NUM")

main :: IO ()
main = execParser opts >>= runSolution
    where
        opts = info (helper <*> solutionOptions) (fullDesc)
