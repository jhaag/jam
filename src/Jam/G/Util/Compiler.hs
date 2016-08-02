module Jam.G.Util.Compiler where

import Jam.Language
import Jam.Util.Core

data GmState = State { getCode  :: GmCode
                     , getStack :: GmStack
                     , getHeap  :: GmHeap
                     , getEnv   :: GmEnv
                     , getStats :: GmStats
                     }

putCode :: GmCode -> GmState -> GmState
putCode i' (State i stack heap env stats) = State i' stack heap env stats

putStack :: GmStack -> GmState -> GmState
putStack stack' (State i stack heap env stats) = State i stack' heap env stats

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (State i stack heap env stats) = State i stack heap' env stats

putEnv :: GmEnv -> GmState -> GmState
putEnv env' (State i stack heap env stats) = State i stack heap env' stats

putStats :: GmStats -> GmState -> GmState
putStats stats' (State i stack heap env stats) = State i stack heap env stats'

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]

type GmCode  = [Instruction]
type GmStack = [Addr]
type GmHeap  = Heap Node
type GmEnv   = Assoc Name Addr
type GmStats = Int

-- Statistics
statInitial :: GmStats
statInitial    = 0

statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1

statGetSteps :: GmStats -> GmStats
statGetSteps s = s

-- Instructions
data Instruction = Unwind
                 | Pushglobal Name
                 | Pushint Int
                 | Push Int
                 | Mkap
                 | Slide Int

instance Eq Instruction where
  Unwind == Unwind                 = True
  (Pushglobal a) == (Pushglobal b) = a == b
  (Pushint a) == (Pushint b)       = a == b
  (Push a) == (Push b)             = a == b
  Mkap == Mkap                     = True
  (Slide a) == (Slide b)           = a == b
  _ == _                           = False

-- Nodes
data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Int GmCode

-- Compilation Specifics
type GmCompiledSc  = (Name, Int, GmCode)
type GmCompiler    = CoreExpr -> GmCompilerEnv -> GmCode
type GmCompilerEnv = Assoc Name Int
