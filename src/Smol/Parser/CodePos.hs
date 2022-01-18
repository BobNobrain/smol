module Smol.Parser.CodePos
    ( CodePos
    , zero
    , incrementCol
    , incrementLine
    , showWithFilename
    , getLine
    , getColumn
    , getCaret
    ) where

import Prelude hiding (getLine)

data CodePos = CodePos
    { getCaret :: Int
    , getColumn :: Int
    , getLine :: Int
    }
    deriving (Eq)

zero :: CodePos
zero = CodePos 0 1 1

incrementCol :: Int -> CodePos -> CodePos
incrementCol n (CodePos caret col ln) = CodePos (caret + n) (col + n) ln

incrementLine :: CodePos -> CodePos
incrementLine (CodePos caret col ln) = CodePos (caret + 1) 1 (ln + 1)


instance Show CodePos where
    show pos = show (getLine pos) ++ ":" ++ show (getColumn pos)

showWithFilename :: FilePath  -> CodePos -> String
showWithFilename filename pos = filename ++ ":" ++ show pos
