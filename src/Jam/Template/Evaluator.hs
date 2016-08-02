module Jam.Template.Evaluator 
  (
    run,
    run'
  ) where

import Jam.Template.Compiler
import Jam.Template.Evaluator.Core
import Jam.Template.Evaluator.Print
import Jam.Parser

run :: String -> String
run = showResults . eval . compile . parse

run' :: String -> String
run' = showResults' . eval' . compile . parse
