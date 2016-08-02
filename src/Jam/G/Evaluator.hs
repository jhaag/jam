module Jam.G.Evaluator
  (
    run
  ) where

import Jam.G.Compiler
import Jam.G.Evaluator.Core
import Jam.G.Evaluator.Print
import Jam.Parser

run :: String -> String
run = showResults . eval . compile . parse
