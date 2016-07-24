module Jam.Parser where

import Jam.Language
import Jam.Parser.Lexer
import Jam.Parser.Syntax

parse :: String -> CoreProgram
parse = syntax . tokenize
