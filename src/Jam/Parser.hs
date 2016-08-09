module Jam.Parser where

import Jam.Language.Jam
import Jam.Parser.Lexer
import Jam.Parser.Syntax

parse :: String -> JamProgram
parse = syntax . tokenize
