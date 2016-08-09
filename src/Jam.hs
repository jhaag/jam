module Jam
  (
    jamMain
  ) where

import Jam.Parser
import Jam.Parser.Lexer
import Jam.Simplify

jamMain :: IO ()
jamMain = do
  prog <- readFile "/home/jhaag/dev/jam/src/jam_test.jam"
  print $ replicate 80 '~'
  print $ tokenize prog
  print $ replicate 80 '~'
  print $ parse prog
  print $ replicate 80 '~'
  print $ toEnriched $ parse prog
