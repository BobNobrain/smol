{-# LANGUAGE OverloadedStrings #-}

module Smol.Module.Name
    ( SmolModule (..)
    , ModuleName (ModuleName)
    , createModuleName
    , ModuleNamePattern
    , createModuleNamePattern
    , matchModuleName
    ) where

import Data.Text (Text, splitOn, isPrefixOf, drop, unpack)

data SmolModule = SmolModule
    { moduleName :: ModuleName
    , content :: ()
    }

newtype ModuleName = ModuleName { unwrapModuleName :: [Text] }

createModuleName :: Text -> ModuleName
createModuleName qualified = ModuleName $ splitOn "." qualified

data ModuleNamePatternPart = LiteralPart Text | StarPart

instance Show ModuleNamePatternPart where
    show (LiteralPart t) = unpack t
    show StarPart = "*"

data ModuleNamePattern = ModuleNamePattern
    { parts :: [ModuleNamePatternPart]
    , isNegative :: Bool
    }

instance Show ModuleNamePattern where
    show p = show $ n ++ join "." (map show (parts p)) where
        n = if isNegative p then "-" else ""
        join :: String -> [String] -> String
        join sep [] = ""
        join sep [p] = p
        join sep (p:ps) = p ++ sep ++ join sep ps

createModuleNamePattern :: Text -> ModuleNamePattern
createModuleNamePattern input =
    if "-" `isPrefixOf` input then
        ModuleNamePattern { parts = createParts (Data.Text.drop 1 input), isNegative = True }
    else
        ModuleNamePattern { parts = createParts input, isNegative = False }
    where
        createParts :: Text -> [ModuleNamePatternPart]
        createParts input = createPart <$> splitOn "." input

        createPart :: Text -> ModuleNamePatternPart
        createPart "*" = StarPart
        createPart t = LiteralPart t


matchModuleName :: ModuleNamePattern -> ModuleName -> Bool
matchModuleName pattern name =
    if isNegative pattern then
        not matches
    else
        matches
    where
        ps = parts pattern
        ns = unwrapModuleName name
        equalLengths = length ps == length ns

        matches = equalLengths && and matchingResults

        matchingResults :: [Bool]
        matchingResults = zipWith matchPart ps ns

        matchPart :: ModuleNamePatternPart -> Text -> Bool
        matchPart StarPart _ = True
        matchPart (LiteralPart p) n = p == n
