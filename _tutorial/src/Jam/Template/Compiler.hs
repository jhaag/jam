module Jam.Template.Compiler
  (
    compile
  ) where

import           Jam.Language
import           Jam.Library.Prelude
import qualified Jam.Template.Library.Prelude as Template
import           Jam.Template.Util.Compiler
import           Jam.Util

compile :: CoreProgram -> TiState
compile program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where scDefs = program ++ preludeDefs ++ extraPreludeDefs
        (initialHeap, globals) = buildInitialHeap scDefs
        initialStack = [mainAddr]
        mainAddr = aLookup globals "main" "main is not defined"

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = flip (foldr allocatePrim) Template.primitives 
                 . foldr allocateSc (hInitial, [])

allocateSc :: CoreScDefn -> (TiHeap, TiGlobals) -> (TiHeap, TiGlobals)
allocateSc (name, args, body) (heap, globals) = (heap', (name, addr):globals)
  where (heap', addr) = hAlloc heap $ NSupercomb name args body

allocatePrim :: (Name, Template.Primitive) -> (TiHeap, TiGlobals) -> (TiHeap, TiGlobals)
allocatePrim (name, prim) (heap, globals) = (heap', (name, addr):globals)
  where (heap', addr) = hAlloc heap $ NPrim name prim
