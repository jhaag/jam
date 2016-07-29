module Jam.Util.Compiler where

import Jam.Language
import Jam.Library.Prelude
import Jam.Util.Core

data Node = NAp Addr Addr                     -- Application
          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
          | NNum Int                          -- Number Literal
          | NPrim Name Primitive              -- Primitive
          | NInd Addr                         -- Indirection to another node in the graph

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- Stack
type TiStack = [Addr]

-- Dump
type TiDump = [TiStack]
initialTiDump = []

--Heap
type TiHeap = Heap Node

-- Globals
type TiGlobals = Assoc Name Addr

-- Stats
type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncStep :: TiStats -> TiStats
tiStatIncStep s = s + 1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats) = (stack, dump, heap, globals, f stats)
