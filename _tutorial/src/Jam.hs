module Jam
  (
    jamMain
  )where

import Jam.G.Evaluator
import Jam.Language
import Jam.Parser.Lexer
import Jam.Parser

jamMain :: IO ()
jamMain = do
  prog <- readFile "/home/jhaag/dev/jam/src/binop_test_1.core"
  --prog <- readFile "/home/jhaag/dev/haskell/jam/src/binop_test_1.core"
  print $ tokenize prog
  putStrLn "--------------------------------------------------------------------------------"
  print $ parse prog
  putStrLn "--------------------------------------------------------------------------------"
  putStrLn $ run' prog
