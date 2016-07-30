module Jam
  (
    jamMain
  )where

import Control.Monad
import Jam.Evaluator
import Jam.Library.Prelude
import Jam.Library.Print
import Jam.Language
import Jam.Parser
--import Jam.Parser.Core
import Jam.Parser.Lexer
--import Jam.Parser.Syntax
import Jam.Util.Parser

jamMain :: IO ()
jamMain = do
  -- for laptop: prog <- readFile "/home/jhaag/dev/jam/src/graph_ex_1.core"
  prog <- readFile "/home/jhaag/dev/haskell/jam/src/binop_test_1.core"
  print $ tokenize prog
  putStrLn "--------------------------------------------------------------------------------"
  print $ parse prog
  putStrLn "--------------------------------------------------------------------------------"
  putStrLn $ run prog
