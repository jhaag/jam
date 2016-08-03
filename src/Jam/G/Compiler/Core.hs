module Jam.G.Compiler.Core where

import Jam.G.Library.Prelude
import Jam.G.Util.Compiler
import Jam.Language
import Jam.Util.Core

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSc
compileSc (name, args, body) = (name, length args, compileR body (zip args [0..]))

compileR :: GmCompiler
compileR exp args = compileE exp args ++ [Update (length args), Pop (length args), Unwind]

compileE :: GmCompiler
compileE (ENum n) args = [Pushint n]
compileE (ELet recursive defs exp) args
  | recursive = compileLetRec compileE defs exp args
  | otherwise = compileLet    compileE defs exp args
compileE (EAp (EVar "negate") e) args = compileE e args ++ [Neg]
compileE (EAp (EAp (EAp (EVar "if") e0) e1) e2) args = compileE e0 args ++ [Cond (compileE e1 args) (compileE e2 args)]
compileE e@(EAp (EAp (EVar op) e0) e1) args = if op `elem` aDomain builtInDyadic
                                                 then compileE e0 args ++ 
                                                      compileE e1 (argOffset 1 args) ++ 
                                                      [aLookup builtInDyadic op "Can't happen"]
                                                 else compileC e args ++ [Eval]
compileE e args = compileC e args ++ [Eval]

compileC :: GmCompiler
compileC (EVar v) args
    | v `elem` (aDomain args) = [Push n]
    | otherwise               = [Pushglobal v]
  where n = aLookup args v ("Can't happen")
compileC (ENum n) args = [Pushint n]
compileC (EAp e1 e2) args = compileC e2 args ++ 
                            compileC e1 (argOffset 1 args) ++
                            [Mkap]
compileC (ELet recursive defs exp) args
  | recursive = compileLetRec compileC defs exp args
  | otherwise = compileLet    compileC defs exp args
compileC _ _ = error "Not implemented yet"

compileLetRec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetRec comp defs exp env = [Alloc (length defs)] ++ compileLetRec' defs env' ++ comp exp env' ++ [Slide (length defs)]
  where env' = compileArgs defs env

compileLetRec' :: [(Name, CoreExpr)] -> GmCompilerEnv -> GmCode
compileLetRec' [] env = []
compileLetRec' ((name, expr):defs) env = compileC expr env ++ [Update (length defs)] ++ compileLetRec' defs env

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs exp env = compileLet' defs env ++ comp exp env' ++ [Slide (length defs)]
  where env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GmCompilerEnv -> GmCode
compileLet' [] env = []
compileLet' ((name, expr):defs) env = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileArgs :: [(Name, CoreExpr)] -> GmCompilerEnv -> GmCompilerEnv
compileArgs defs env = zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

argOffset :: Int -> GmCompilerEnv -> GmCompilerEnv
argOffset n args = [(v, n + m) | (v, m) <- args]
