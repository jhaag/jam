module Jam.G.Util.Compiler where

import Jam.Language
import Jam.Util.Core

data GmState = State { getOutput :: GmOutput
                     , getCode   :: GmCode
                     , getStack  :: GmStack
                     , getDump   :: GmDump
                     , getHeap   :: GmHeap
                     , getEnv    :: GmEnv
                     , getStats  :: GmStats
                     }

putOutput :: GmOutput -> GmState -> GmState
putOutput output' (State output i stack dump heap env stats) = State output' i stack dump heap env stats

putCode :: GmCode -> GmState -> GmState
putCode i' (State output i stack dump heap env stats) = State output i' stack dump heap env stats

putStack :: GmStack -> GmState -> GmState
putStack stack' (State output i stack dump heap env stats) = State output i stack' dump heap env stats

putDump :: GmDump -> GmState -> GmState
putDump dump' (State output i stack dump heap env stats) = State output i stack dump' heap env stats

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (State output i stack dump heap env stats) = State output i stack dump heap' env stats

putEnv :: GmEnv -> GmState -> GmState
putEnv env' (State output i stack dump heap env stats) = State output i stack dump heap env' stats

putStats :: GmStats -> GmState -> GmState
putStats stats' (State output i stack dump heap env stats) = State output i stack dump heap env stats'

initialCode :: GmCode
initialCode = [Pushglobal "main", Eval]

type GmOutput = String
type GmCode  = [Instruction]
type GmStack = [Addr]
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)
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
                 | Update Int
                 | Pop Int
                 | Slide Int
                 | Alloc Int
                 | Eval
                 | Add | Sub | Mul | Div | Neg
                 | Eq | Ne | Lt | Le | Gt | Ge
                 | Cond GmCode GmCode
                 | Pack Int Int
                 | Casejump [(Int, GmCode)]
                 | Split Int
                 | Print
                 deriving (Eq)

builtInDyadic :: Assoc Name Instruction
builtInDyadic = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div),
                 ("==", Eq), ("/=", Ne), (">=", Ge), ("<=", Le),
                 (">", Gt), ("<", Lt)]

compiledPrimitives :: [GmCompiledSc]
compiledPrimitives = [
                       ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),
                       ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
                       ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
                       ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
                       ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
                       ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
                       ("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
                       ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
                       ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
                       (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
                       (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
                     ]

-- Nodes
data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Int GmCode
          | NInd Addr
          | NConstr Int [Addr]
          deriving (Eq)

-- Compilation Specifics
type GmCompiledSc  = (Name, Int, GmCode)
type GmCompiler    = CoreExpr -> GmCompilerEnv -> GmCode
type GmCompilerEnv = Assoc Name Int
