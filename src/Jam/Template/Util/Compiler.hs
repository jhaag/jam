module Jam.Template.Util.Compiler where

import Jam.Language
import Jam.Template.Library.Prelude
import Jam.Util.Core

data Node = NAp Addr Addr                     -- Application
          | NSupercomb Name [Name] CoreExpr   -- Supercombinator
          | NNum Int                          -- Number Literal
          | NPrim Name Primitive              -- Primitive
          | NInd Addr                         -- Indirection to another node in the graph
          | NData Int [Addr]                  -- Tag, list of components
          deriving (Eq)

instance Ord Node where
  (NAp _ _) `compare` (NAp _ _)                   = undefined
  (NSupercomb _ _ _) `compare` (NSupercomb _ _ _) = undefined
  (NNum n1) `compare` (NNum n2)                   = n1 `compare` n2
  (NPrim _ _) `compare` (NPrim _ _)               = undefined
  (NInd _) `compare` (NInd _)                     = undefined
  (NData a1 ds1) `compare` (NData a2 ds2)        = if a1 /= a2
                                                       then undefined
                                                       else let res = (dropWhile (== EQ)) $ zipWith compare ds1 ds2
                                                             in if null res
                                                                   then EQ
                                                                   else head res

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
