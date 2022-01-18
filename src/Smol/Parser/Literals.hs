{-# LANGUAGE OverloadedStrings #-}
module Smol.Parser.Literals
    ( SmolQuoteType (..)
    , getQuoteChar
    , getQuoteByChar
    , StringLiteral (..)
    , NumericLiteral (..)
    ) where

import Data.Text (Text, replace, pack, unpack)


data SmolQuoteType = DoubleQoute | SingleQuote | BacktickQuote
    deriving (Eq)

getQuoteChar :: SmolQuoteType -> Char
getQuoteChar DoubleQoute = '"'
getQuoteChar SingleQuote = '\''
getQuoteChar BacktickQuote = '`'

getQuoteByChar :: Char -> SmolQuoteType
getQuoteByChar '\'' = SingleQuote
getQuoteByChar '`' = BacktickQuote
getQuoteByChar _ = DoubleQoute

instance Show SmolQuoteType where
    show DoubleQoute = "<\">"
    show SingleQuote = "<'>"
    show BacktickQuote = "<`>"

data StringLiteral = StringLiteral
    { quoteType :: SmolQuoteType
    , stringContent :: Text
    }

instance Show StringLiteral where
    show (StringLiteral q c) = qq ++ unpack escaped ++ qq where
        qq = [getQuoteChar q]
        tq = pack qq
        esq = pack ("\\" ++ qq)
        escaped = replace (pack qq) esq c


data NumericLiteral = NumericLiteral
    { wholePart :: Text
    , fracPart :: Text
    , exponent :: Text
    , prefix :: Text -- like "0x" in 0xFF
    , suffix :: Text -- like "km" in 12km
    }
