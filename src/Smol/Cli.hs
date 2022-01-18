module Smol.Cli (cli) where

import Smol.Project (readProject)
import System.Exit (exitWith, ExitCode (..), exitSuccess)
import Smol.Project.Files (discoverSources)

cli :: IO ()
cli = do
    result <- readProject
    case result of
        (Left err) -> do
            putStrLn "Not a project"
            putStrLn err
            exitWith $ ExitFailure 1
        (Right project) -> do
            print project
            exitSuccess
