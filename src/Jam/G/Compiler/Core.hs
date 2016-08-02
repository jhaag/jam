module Jam.G.Compiler.Core where

import Jam.G.Util.Compiler
import Jam.Language
import Jam.Util.Core

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSc
compileSc (name, args, body) = (name, length args, compileR body (zip args [0..]))

compileR :: GmCompiler
compileR exp args = compileC exp args ++ [Slide (length args + 1), Unwind]

compileC :: GmCompiler
compileC (EVar v) args
    | v `elem` (aDomain args) = [Push n]
    | otherwise               = [Pushglobal v]
  where n = aLookup args v ("Can't happen")
compileC (ENum n) args = [Pushint n]
compileC (EAp e1 e2) args = compileC e2 args ++ 
                            compileC e1 (argOffset 1 args) ++
                            [Mkap]
compileC _ _ = error "Not implemented yet"

argOffset :: Int -> GmCompilerEnv -> GmCompilerEnv
argOffset n args = [(v, n + m) | (v, m) <- args]
