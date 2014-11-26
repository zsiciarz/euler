module EulerMain where

import Options.Applicative
import Solutions (SolutionOptions (..), runSolution)

solutionOptions :: Parser SolutionOptions
solutionOptions = SolutionOptions <$> option auto (long "problem" <> metavar "NUM")

eulerMain :: IO ()
eulerMain = execParser opts >>= runSolution
    where
        opts = info (helper <*> solutionOptions) fullDesc
