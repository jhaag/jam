module Jam.Evaluator.Core
  (
    eval
  ) where

import Jam.Language
import Jam.Util

eval :: TiState -> [TiState]
eval state = state:future
  where future
          | tiFinal state = []
          | otherwise     = eval nextState
        nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncStep state

tiFinal :: TiState -> Bool
tiFinal ([addr], dump, heap, globals, stats) = isDataNode (hLookup heap addr)
tiFinal ([], _, _, _, _) = error "Empty Stack!"
tiFinal _                = False -- stack contains more than one address

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode _        = False

-- Step function for evaluation
step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) = dispatch (hLookup heap (head stack))
  where dispatch (NNum n)                   = numStep state n
        dispatch (NInd a)                   = indStep state a
        dispatch (NAp a1 a2)                = apStep state a1 a2
        dispatch (NSupercomb sc args body)  = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

indStep :: TiState -> Addr -> TiState
indStep (_:stack, dump, heap, globals, stats) a = (a:stack, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2
  = (a1:stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) name args body
    = (stack', dump, heap'', globals, stats)
  where stack' = addr:drop (length args + 1) stack
        rootAddr = stack !! length args
        (heap', addr) = instantiate body heap env
        heap'' = hUpdate heap' rootAddr $ NInd addr
        env = argBindings ++ globals
        argBindings = zip args (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map getArg stack
  where getArg addr = let (NAp fun arg) = hLookup heap addr
                       in arg

instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiate (EVar v) heap env               = (heap, aLookup env v err)
  where err = "Undefined name " ++ show v
instantiate (ENum n) heap env               = hAlloc heap (NNum n)
instantiate (EConstr tag arity) heap env    = instantiateConstr  tag arity heap env
instantiate (EAp e1 e2) heap env            = hAlloc heap'' (NAp a1 a2)
  where (heap', a1)   = instantiate e1 heap env
        (heap'', a2)  = instantiate e2 heap' env
instantiate (ELet isRec defs body) heap env = instantiate body heap' env'
  where instantiatedDefs :: [(TiHeap, Addr)]
        instantiatedDefs 
          = tail $ scanl (\(heap', _) (_, exp) -> (flip (`instantiate` heap') env') exp) 
                         (heap, hNull)
                         defs
        addrs = map snd instantiatedDefs
        heap' = (fst . last) instantiatedDefs
        env'  = zip (map fst defs) addrs ++ env
instantiate (ECase exp alts) heap env       = error "Can't instantiate case exprs."
instantiate (ELam args body) heap env       = instantiateLam args body heap env

instantiateConstr tag arity heap env    = error "Can't instantiate constructors yet."
instantiateLam args body heap env       = error "Can't instantiate lambdas yet."
