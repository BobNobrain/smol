{-# LANGUAGE OverloadedStrings #-}
module Smol.Parser.Tokenizer
    ( tokenize
    ) where

import Data.Char (isAlpha, isNumber, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Smol.Parser.CodePosRange as CodePosRange
import Smol.Parser.Tokens (SmolToken (..), SmolTokenType (..), createIdentifierToken)


tokenize :: FilePath -> Text -> [SmolToken]
tokenize filename source = [] where
    showPos = CodePosRange.showWithFilename filename

    folded :: TokenizerAccumulator
    folded = foldl accumulateNextChar emptyAccumulator (T.unpack source)

data TokenizerAccumulator = TokenizerAccumulator
    { result :: [SmolToken]
    , buffer :: String
    , currentTokenType :: Maybe SmolTokenType
    }

data BasicTokenType
    = BTIdentifier
    | BTNumeral
    | BTString
    | BTOperator
    | BTParen

emptyAccumulator :: TokenizerAccumulator
emptyAccumulator = TokenizerAccumulator { result = [], buffer = "", currentTokenType = Nothing }

accumulateNextChar :: TokenizerAccumulator -> Char -> TokenizerAccumulator
accumulateNextChar acc next = acc

getTokenTypeByFirstChar :: Char -> Maybe BasicTokenType
getTokenTypeByFirstChar c
    | isAlpha c = Just BTIdentifier
    | isNumber c = Just BTNumeral
    | c `elem` _STR_QUOTES = Just BTString
    | c `elem` _OPERATOR_START_CHARS = Just BTOperator
    | otherwise = Nothing

eatToken :: BasicTokenType -> Text -> (SmolTokenType, Int)
eatToken BTIdentifier rest = (createIdentifierToken (eatId rest), T.length (eatId rest)) where
    eatId :: Text -> Text
    eatId = T.takeWhile (\c -> not (isSpace c) && (c /= '.'))

-- isStartCharOf :: BasicTokenType -> Char -> Bool
-- isStartCharOf BTIdentifier c = isAlpha c
-- isStartCharOf BTNumeral

-- isContinuationCharOf :: BasicTokenType -> Char -> Bool
-- isContinuationCharOf BTIdentifier c = not (isSpace c) && (c /= '.')

_STR_QUOTES :: String
_STR_QUOTES = "'`\""
_OPERATOR_START_CHARS :: String
_OPERATOR_START_CHARS = "!$%^&*-=+:|~?"
_OPERATOR_CHARS :: String
_OPERATOR_CHARS = "" ++ _OPERATOR_START_CHARS
_PAREN_CHARS :: String
_PAREN_CHARS = "(){}[]"
