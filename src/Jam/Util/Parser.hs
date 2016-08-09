module Jam.Util.Parser where

import            Jam.Language.Jam
import qualified  Data.Char (isAlpha, isAlphaNum, isDigit)

type Line   = Int
type Token  = (Line, String)

isAlpha, isAlphaNum, isDigit, isIdChar, isNewline, isSpace, isSymbol :: Char -> Bool
isAlpha     = Data.Char.isAlpha
isAlphaNum  = Data.Char.isAlphaNum
isDigit     = Data.Char.isDigit
isIdChar c  = isAlphaNum c || (c == '_')
isNewline   = (==) '\n'
isSpace     = flip elem " \t"
isSymbol    = flip elem operatorSymbols
