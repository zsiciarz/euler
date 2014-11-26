module EulerMain where

import Options.Applicative
import Solutions (SolutionOptions (..), runSolution)

solutionOptions :: Parser SolutionOptions
solutionOptions = SolutionOptions <$> option auto (long "problem" <> metavar "NUM")

eulerMain :: IO ()
eulerMain = do
    result <- execParser opts >>= runSolution
    case result of
        Just solution -> print solution
        Nothing -> print "No solution"
    where
        opts = info (helper <*> solutionOptions) fullDesc
