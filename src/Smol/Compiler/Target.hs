{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smol.Compiler.Target
    ( SupportedLanguage(..)
    , CompilerOutputType(..)
    , CompilerTarget
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText, withObject, (.:?), (.:))
import qualified Data.Aeson as JSON (Value (..), Object, Array)
import Data.Aeson.Types (parse, Parser)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (toLower, Text)
import Smol.Module.Name (ModuleNamePattern, createModuleNamePattern)


data SupportedLanguage
    = SupportedLanguageIL
    | SupportedLanguageTypeScript
    | SupportedLanguageC
    deriving (Show, Eq)

instance FromJSON SupportedLanguage where
    parseJSON = withText "SupportedLanguage" $ \s -> case toLower s of
        "il" -> return SupportedLanguageIL
        "ts" -> return SupportedLanguageTypeScript
        "typescript" -> return SupportedLanguageTypeScript
        "c" -> return SupportedLanguageC
        _ -> fail $ "Unknown supported language" ++ show s

data CompilerOutputType
    = BinaryOutput
    | BundleOutput SupportedLanguage
    | FileTreeOutput SupportedLanguage
    deriving (Show, Eq)

data CompilerTarget
    = ExecutableTarget { path :: FilePath, entrypoint :: Text, output :: CompilerOutputType }
    | LibraryTarget { path :: FilePath, include :: [ModuleNamePattern], output :: CompilerOutputType }
    deriving (Show)

instance FromJSON CompilerTarget where
    parseJSON = withObject "CompilerTarget" (
        \obj -> do
            targetType :: Text <- obj .: "type"
            outputPath :: FilePath <- obj .: "path"
            case targetType of
                "executable" -> parseExecutable outputPath obj
                "lib" -> parseLibrary outputPath obj
                _ -> fail $ "Unknown compiler target type " ++ show targetType
        )
        where
            parseExecutable :: FilePath -> JSON.Object  -> Parser CompilerTarget
            parseExecutable outputPath obj = do
                entrypointModuleName :: Text <- obj .: "entrypoint"
                outputType <- parseOutputType obj
                return $ ExecutableTarget
                    { path = outputPath
                    , entrypoint = entrypointModuleName
                    , output = outputType
                    }

            parseLibrary :: FilePath -> JSON.Object  -> Parser CompilerTarget
            parseLibrary outputPath obj = do
                includePatterns :: JSON.Value <- obj .: "include"
                parsed <- parseStringArrayOrSingle includePatterns
                outputType <- parseOutputType obj
                return $ LibraryTarget
                    { path = outputPath
                    , include = map createModuleNamePattern parsed
                    , output = outputType
                    }

            parseOutputType :: JSON.Object -> Parser CompilerOutputType
            parseOutputType obj = do
                isBundled :: Maybe Bool <- obj .:? "bundle"
                lang :: Maybe SupportedLanguage <- obj .:? "output"
                return $ constructOutputType lang (fromMaybe False isBundled)

            constructOutputType :: Maybe SupportedLanguage -> Bool -> CompilerOutputType
            constructOutputType Nothing _ = BinaryOutput
            constructOutputType (Just lang) False = FileTreeOutput lang
            constructOutputType (Just lang) True = BundleOutput lang

            constructIncludePatterns :: [Text] -> [ModuleNamePattern]
            constructIncludePatterns ts = map createModuleNamePattern ts

            parseStringArrayOrSingle :: JSON.Value -> Parser [Text]
            parseStringArrayOrSingle (JSON.String txt) = return [txt]
            parseStringArrayOrSingle (JSON.Array entries) = do
                vector <- traverse checkIfString entries
                return $ toList vector
            parseStringArrayOrSingle _ = fail "Expected a single string or a string array"

            checkIfString :: JSON.Value -> Parser Text
            checkIfString = withText "string" return
