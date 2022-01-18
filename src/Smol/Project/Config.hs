{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Smol.Project.Config
    ( ProjectConfig
    , readConfigFileFrom
    ) where

import System.FilePath (FilePath, (</>))
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?), eitherDecodeFileStrict)
import Data.Text (Text)
import Smol.Compiler.Target (CompilerTarget)
import Data.Maybe (fromMaybe)


data ProjectConfig = ProjectConfig
    { projectName :: Text
    , dependencies :: [Text]
    , targets :: [CompilerTarget]
    }
    deriving (Show)

instance FromJSON ProjectConfig where
    parseJSON = withObject "ProjectConfig" (
        \obj -> do
            name :: Text <- obj .: "name"
            deps :: Maybe [Text] <- obj .:? "deps"
            targets :: [CompilerTarget] <- obj .: "targets"
            return $ ProjectConfig
                { projectName = name
                , dependencies = fromMaybe [] deps
                , targets = targets
            }
        )

readConfigFileFrom :: FilePath -> IO (Either String ProjectConfig)
readConfigFileFrom = eitherDecodeFileStrict
