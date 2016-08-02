module Jam.Template.Evaluator.Core
  (
    eval,
    eval'
  ) where

import Debug.Trace
import Jam.Language
import Jam.Library.Prelude
import Jam.Template.Library.Prelude
import Jam.Template.Util.Compiler
import Jam.Util

eval :: TiState -> [TiState]
eval state = state:future
  where future
          | tiFinal state = []
          | otherwise     = eval nextState
        nextState = doAdmin (step state)

eval' :: TiState -> TiState
eval' state = future
  where future
          | tiFinal state = state
          | otherwise     = eval' nextState
        nextState = doAdmin (step state) 

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncStep state

tiFinal :: TiState -> Bool
tiFinal ([addr], dump, heap, globals, stats) = null dump && isDataNode (hLookup heap addr)
tiFinal ([], _, _, _, _) = error "Empty Stack!"
tiFinal _                = False -- stack contains more than one address

isDataNode :: Node -> Bool
isDataNode (NNum n)     = True
isDataNode (NData t d)  = True
isDataNode _            = False

-- Step function for evaluation
step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) = dispatch (hLookup heap (head stack))
  where dispatch (NNum n)                   = numStep state n
        dispatch (NInd a)                   = indStep state a
        dispatch (NAp a1 a2)                = apStep state a1 a2
        dispatch (NSupercomb sc args body)  = scStep state sc args body
        dispatch (NPrim n p)                = primStep state n p
        dispatch (NData t components)       = dataStep state t components

primStep :: TiState -> Name -> Primitive -> TiState
primStep state name prim = case prim of
                             Neg              -> primNeg state
                             Add              -> primAdd state
                             Sub              -> primSub state
                             Mul              -> primMul state
                             Div              -> primDiv state
                             (PrimConstr t a) -> primConstr state t a
                             If               -> primIf state
                             G                -> primG state
                             GE               -> primGE state
                             L                -> primL state
                             LE               -> primLE state
                             E                -> primE state
                             NE               -> primNE state

primConstr :: TiState -> Int -> Int -> TiState
primConstr (stack, dump, heap, env, stats) tag arity = (stack', dump, heap', env, stats)
  where outerAp = if length stack < arity + 1
                     then error "Too few arguments to data constructor."
                     else stack !! arity
        args = constrArgs stack heap outerAp
        heap' = if length args == arity
                   then hUpdate heap outerAp $ NData tag args
                   else error "Argument error for data constructor."
        stack' = drop arity stack

constrArgs :: TiStack -> TiHeap -> Addr -> [Addr]
constrArgs stack heap addr
  | addr == head stack = []
  | otherwise          = constrArgs stack heap next ++ [arg]
      where (NAp next arg) = hLookup heap addr

primNeg, primAdd, primSub, primMul, primDiv, primIf, primG, primGE, primL, primLE, primE, primNE :: TiState -> TiState
primNeg (stack@(prim:part:rest), dump, heap, env, stats)
  = let (NAp neg arg) = hLookup heap part
        node = hLookup heap arg
     in if isDataNode node
           then let (NNum n) = node
                    heap' = hUpdate heap part (NNum (-n))
                 in (part:rest, dump, heap', env, stats)
           else ([arg], stack:dump, heap, env, stats)
primIf (stack@(prim:ifAp:thenAp:elseAp:rest), dump, heap, env, stats)
  = let (NAp if' cond) = hLookup heap ifAp
        condNode = hLookup heap cond
     in if isDataNode condNode
           then case condNode of
                     --true
                     (NData 2 []) -> let (NAp then' resAddr) = hLookup heap thenAp
                                         resNode = hLookup heap resAddr
                                      in if isDataNode resNode
                                            then let heap' = hUpdate heap elseAp resNode
                                                  in (elseAp:rest, dump, heap', env, stats)
                                            else ([resAddr], stack:dump, heap, env, stats)
                     --false
                     (NData 1 []) -> let (NAp else' resAddr) = hLookup heap elseAp
                                         resNode = hLookup heap resAddr
                                      in if isDataNode resNode
                                            then let heap' = hUpdate heap elseAp resNode
                                                  in (elseAp:rest, dump, heap', env, stats)
                                            else ([resAddr], stack:dump, heap, env, stats)
           else ([cond], stack:dump, heap, env, stats)
primAdd state = primArith state (+)
primSub state = primArith state (-)
primMul state = primArith state (*)
primDiv state = primArith state div
primG state   = primComp state (>)
primGE state  = primComp state (>=)
primL state   = primComp state (<)
primLE state  = primComp state (<=)
primE state   = primComp state (==)
primNE state  = primComp state (/=)

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state f = primDyadic state (\n1 n2 -> let (NNum n1') = n1
                                                    (NNum n2') = n2
                                                 in NNum $ f n1' n2')

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state f = primDyadic state (\n1 n2 -> let (NNum n1') = n1
                                                   (NNum n2') = n2
                                                in if f n1' n2'
                                                      then NData 2 []
                                                      else NData 1 [])

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (stack@(prim:lhs:rhs:rest), dump, heap, env, stats) f
  = let (NAp op a1) = hLookup heap lhs
        n1 = hLookup heap a1
     in if isDataNode n1
           then let (NAp part a2) = hLookup heap rhs
                    n2 = hLookup heap a2
                 in if isDataNode n2
                       then let heap' = hUpdate heap rhs (f n1 n2)
                             in (rhs:rest, dump, heap', env, stats)
                       else ([a2], stack:dump, heap, env, stats)
           else ([a1], stack:dump, heap, env, stats)

numStep :: TiState -> Int -> TiState
numStep (ns, [], heap, globals, stats) _      = error "Number applied as a function."
numStep ([n], d:ump, heap, globals, stats) _  = (d, ump, heap, globals, stats)
numStep (ns, dump, heap, globals, stats) _    = error "Number applied as a function."

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep (ds, [], heap, globals, stats) _ _       = error "Data applied as a function."
dataStep ([d], d':ump, heap, globals, stats) _ _  = (d', ump, heap, globals, stats)
dataStep (ds, dump, heap, globals, stats) _ _     = error "Data applied as a function."

indStep :: TiState -> Addr -> TiState
indStep (_:stack, dump, heap, globals, stats) a = (a:stack, dump, heap, globals, stats)

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1:stack, dump, heap', globals, stats)
  where heap' = case hLookup heap a2 of
                     (NInd addr)  -> hUpdate heap a2 (hLookup heap addr)
                     _            -> heap

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) name args body
    = (stack', dump, heap'', globals, stats)
  where stack' = rootAddr:drop (length args + 1) stack
        rootAddr = stack !! length args
        heap'' = instantiate' body rootAddr heap env
        env = argBindings ++ globals
        argBindings = zip args (getArgs heap stack)

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map getArg stack
  where getArg addr = let (NAp fun arg) = hLookup heap addr
                       in arg

instantiate' :: CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiate' (EVar v) addr heap env               = hUpdate heap addr $ NInd $ aLookup env v err
  where err = "Undefined name " ++ show v
instantiate' (ENum n) addr heap env               = hUpdate heap addr $ NNum n
instantiate' (EConstr tag arity) addr heap env    = hUpdate heap addr $ NPrim "Pack" (PrimConstr tag arity)
instantiate' (EAp e1 e2) addr heap env            = hUpdate heap'' addr (NAp a1 a2)
  where (heap', a1)   = instantiate e1 heap env
        (heap'', a2)  = instantiate e2 heap' env
instantiate' (ELet isRec defs body) addr heap env = instantiate' body addr heap' env'
  where instantiatedDefs :: [(TiHeap, Addr)]
        instantiatedDefs 
          = tail $ scanl (\(heap', _) (_, exp) -> (flip (`instantiate` heap') env') exp) 
                         (heap, hNull)
                         defs
        addrs = map snd instantiatedDefs
        heap' = (fst . last) instantiatedDefs
        env'  = zip (map fst defs) addrs ++ env
instantiate' (ECase exp alts) addr heap env       = error "Cant instantiate case exprs."
instantiate' (ELam args body) addr heap env       = instantiateLam args body heap env

instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiate (EVar v) heap env               = (heap, aLookup env v err)
  where err = "Undefined name " ++ show v
instantiate (ENum n) heap env               = hAlloc heap (NNum n)
instantiate (EConstr tag arity) heap env    = hAlloc heap (NPrim "Pack" (PrimConstr tag arity))
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

instantiateLam args body heap env       = error "Can't instantiate lambdas yet."
