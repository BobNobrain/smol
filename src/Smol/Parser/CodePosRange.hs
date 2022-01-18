module Smol.Parser.CodePosRange
    ( CodePosRange (..)
    , showWithFilename
    ) where

import qualified Smol.Parser.CodePos as CodePos
import Smol.Parser.CodePos (CodePos)

data CodePosRange = CodePosRange
    { getRangeStart :: CodePos
    , getRangeEnd :: CodePos
    } deriving (Eq)

instance Show CodePosRange where
    show (CodePosRange start end)
        | start == end =
            show start
        | CodePos.getLine start == CodePos.getLine end =
            show (CodePos.getLine start) ++ ":" ++ show (CodePos.getColumn start) ++ "-" ++ show (CodePos.getColumn end)
        | otherwise =
            show start ++ "-" ++ show end


showWithFilename :: FilePath  -> CodePosRange -> String
showWithFilename filename range = filename ++ ":" ++ show range

emptyRange :: CodePos -> CodePosRange
emptyRange pos = CodePosRange
    { getRangeStart = pos
    , getRangeEnd = pos
    }

getLength :: CodePosRange -> Int
getLength r = CodePos.getCaret (getRangeEnd r) - CodePos.getCaret (getRangeStart r)
