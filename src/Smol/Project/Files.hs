module Smol.Project.Files
    ( SourceFile
    , discoverSources
    ) where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)


data SourceFile
    = SmolSourceFile { path :: FilePath }
    | AssetFile { path :: FilePath }
    deriving (Show, Eq)

discoverSources :: FilePath -> IO [SourceFile]
discoverSources root = do
    content <- listDirectory root
    childEntries <- mapM toSourceFile content
    return $ concat childEntries
    where
        toSourceFile :: FilePath -> IO [SourceFile]
        toSourceFile path = do
            isDir <- doesDirectoryExist path
            if isDir then
                discoverSources path
            else if takeExtension path == ".smol" then
                return [SmolSourceFile path]
            else
                return [AssetFile path]
