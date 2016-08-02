module Jam.G.Compiler
  (
    compile
  ) where

import Jam.G.Compiler.Core
import Jam.G.Util.Compiler
import Jam.Language
import Jam.Library.Prelude
import Jam.Util.Core

compile :: CoreProgram -> GmState
compile program = State initialCode [] heap env statInitial
  where (heap, env) = buildInitialHeap (program ++ preludeDefs ++ extraPreludeDefs)

buildInitialHeap :: CoreProgram -> (GmHeap, GmEnv)
buildInitialHeap = foldr allocateSc (hInitial, []) . (map compileSc)

allocateSc :: GmCompiledSc -> (GmHeap, GmEnv) -> (GmHeap, GmEnv)
allocateSc (name, nargs, instructions) (heap, env) = (heap', (name, addr):env)
  where (heap', addr) = hAlloc heap (NGlobal nargs instructions)
