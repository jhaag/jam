module Jam.Evaluator 
  (
    run
  ) where

import Jam.Compiler
import Jam.Evaluator.Core
import Jam.Evaluator.Print
import Jam.Parser

run :: String -> String
run = showResults . eval . compile . parse
