{-# LANGUAGE OverloadedStrings #-}
module Smol.Parser.Tokens
    ( SmolTokenType (..)
    , SmolKeyword (..)
    , createIdentifierToken
    , SmolQuoteType (..)
    , SmolToken (..)
    ) where

import Data.Text (Text)
import Smol.Parser.CodePos (CodePos)
import Smol.Parser.CodePosRange (CodePosRange)
import Smol.Parser.Literals (SmolQuoteType)


data SmolKeyword
    = SKWModule
    | SKWImport
    | SKWType
    | SKWData
    | SKWUnion
    | SKWNumeral
    | SKWReturns
    | SKWConstraint
    deriving (Eq)

instance Show SmolKeyword where
    show SKWModule = show "module"
    show SKWImport = show "import"
    show SKWType = show "type"
    show SKWData = show "data"
    show SKWUnion = show "union"
    show SKWNumeral = show "numeral"
    show SKWReturns = show "returns"
    show SKWConstraint = show "constraint"

createIdentifierToken :: Text -> SmolTokenType
createIdentifierToken "module" = KeywordToken SKWModule
createIdentifierToken "import" = KeywordToken SKWImport
createIdentifierToken "type" = KeywordToken SKWType
createIdentifierToken "data" = KeywordToken SKWData
createIdentifierToken "union" = KeywordToken SKWUnion
createIdentifierToken "numeral" = KeywordToken SKWNumeral
createIdentifierToken "returns" = KeywordToken SKWReturns
createIdentifierToken "constraint" = KeywordToken SKWConstraint
createIdentifierToken id = IdentifierToken id

data SmolTokenType
    = KeywordToken SmolKeyword
    | OperatorToken Text
    | IdentifierToken Text
    | OpenBraceToken
    | CloseBraceToken
    | OpenParenToken
    | CloseParenToken
    | OpenBracketToken
    | CloseBracketToken
    -- | OpenAngularToken
    -- | CloseAngularToken
    | LambdaToken
    | StringLiteralToken SmolQuoteType Text
    | NumericLiteralToken Text -- TODO: introduce a data type for numeric literal
    deriving (Show, Eq)

data SmolToken = SmolToken
    { tokenType :: SmolTokenType
    , tokenPos :: CodePosRange
    }
    deriving (Show, Eq)

-- instance Show SmolToken where
--     show (KeywordToken kw) = show kw ++ " keyword"
--     show (OperatorToken op) = show op ++ " operator"
--     show (NameToken n) = show n ++ " name"
