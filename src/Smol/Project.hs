module Smol.Project
    ( Project
    , readProject
    ) where

import System.Directory (getCurrentDirectory, doesFileExist, canonicalizePath)
import System.FilePath (FilePath, (</>))
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?), eitherDecodeFileStrict)
import Data.Text (Text)
import Smol.Compiler.Target (CompilerTarget)
import Data.Maybe (fromMaybe)
import Smol.Project.Config (ProjectConfig, readConfigFileFrom)
import Smol.Project.Files (SourceFile, discoverSources)


data Project = Project
    { root :: FilePath
    , config :: ProjectConfig
    , sources :: [SourceFile]
    }
    deriving (Show)


_PROJECT_JSON :: FilePath
_PROJECT_JSON = "project.json"

_SRC_DIR :: FilePath
_SRC_DIR = "src"


readProject :: IO (Either String Project)
readProject = do
    root <- findProjectRoot
    case root of
        Nothing -> return $ Left "Neither current directory, nor any of its parent directories contain project.json"
        Just projectRoot -> readProjectFrom projectRoot


readProjectFrom :: FilePath -> IO (Either String Project)
readProjectFrom projectRoot = do
    confOrErr <- readConfigFileFrom (projectRoot </> _PROJECT_JSON)
    case confOrErr of
        Left err -> return $ Left ("Failed to parse project.json: " ++ err)
        Right conf -> do
            sources <- discoverSources (projectRoot </> _SRC_DIR)
            return $ Right $ Project
                { root = projectRoot
                , config = conf
                , sources = sources
                }


findProjectRoot :: IO (Maybe FilePath)
findProjectRoot = do
    cwd <- getCurrentDirectory
    findRoot cwd
    where
        findRoot :: FilePath -> IO (Maybe FilePath)
        findRoot "/" = return Nothing
        findRoot dir = do
            let candidate = dir </> _PROJECT_JSON
            exists <- doesFileExist candidate
            if exists then
                return $ Just dir
            else do
                upper <- canonicalizePath (dir </> "..")
                findRoot upper
