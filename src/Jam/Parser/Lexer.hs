module Jam.Parser.Lexer
  (
    tokenize
  ) where

import Jam.Language.Jam
import Jam.Util.Parser

tokenize :: String -> [Token]
tokenize = tokenize' 1

tokenize' :: Int -> String -> [Token]
tokenize' line [] = []
tokenize' line (c:cs)
  | isNewline c   = tokenize' (line + 1) cs
  | isSpace c     = tokenize' line cs
  | (not (null cs) && c == '-' && (head cs) == '-')  
                  = tokenize' (line + 1) $ dropWhile (/= '\n') cs
  | isSymbol c    = let symTok = c:takeWhile isSymbol cs
                        rest   = dropWhile isSymbol cs
                     in (line, symTok):tokenize' line rest
  | isDigit c     = let numTok = c:takeWhile isDigit cs
                        rest   = dropWhile isDigit cs
                     in (line, numTok):tokenize' line rest
  | isAlpha c     = let varTok = c:takeWhile isIdChar cs
                        rest = dropWhile isIdChar cs
                     in (line, varTok):tokenize' line rest
  | otherwise     = (line, [c]):tokenize' line cs
