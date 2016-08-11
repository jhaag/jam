module Jam.Parser.Syntax
  (
    syntax
  ) where

import Control.Applicative
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Jam.Language.Jam
import Jam.Parser.Core
import Jam.Util.Helpers
import Jam.Util.Parser

--------------------------------------------------------------------------------
syntax :: [Token] -> JamProgram
syntax = runParse pProgram

pProgram :: Parser JamProgram
pProgram = pSc `sep1` string ";"

pSc :: Parser JamScDefn
pSc = pGuard
  <|> do name <- variable
         args <- many pPattern
         string "="
         expr <- pExpr
         return (name, args, expr)

pGuard :: Parser JamScDefn
pGuard = do
  name <- variable
  args <- many pPattern
  guards <- pGuardDef `sep1` string ";"
  let (ECase cond alts) = last guards
      rest      = init guards
   in return (name,
              args,
              foldr (\(ECase cond' alts') subCase -> ECase cond' (alts ++ [(1, [], subCase)])) 
                    (ECase cond (alts ++ [(2, [], EVar "ERROR")])) 
                    rest
             )

pGuardDef :: Parser JamExpr
pGuardDef = do
  string "|"
  cond <- pExpr -- should evaluate to a bool
  string "="
  body <- pExpr
  return $ ECase cond [(2, [], body)]

pExpr :: Parser JamExpr
pExpr = pLet
    <|> pCase
    <|> pLam
    <|> pAp
    <|> pOperator

pLet :: Parser JamExpr
pLet = do
  letKey  <- keyword -- 'let' or 'letrec'
  defs    <- pLetDef `sep1` string ";"
  keyword -- 'in'
  body    <- pExpr
  return $ ELet (letKey == "letrec") defs body

pLetDef :: Parser (JamExpr, JamExpr)
pLetDef = fmap scdefToLam pSc

pCase :: Parser JamExpr
pCase = do
  keyword -- 'case'
  caseExpr <- pExpr
  keyword -- 'of'
  alts     <- pAlter `sep1` string ";"
  return $ ECase caseExpr alts

--TODO: Change this in the future once I deal with structured data in a more reasonable way
pAlter :: Parser JamAlt
pAlter = do
  string "<"
  tag  <- fmap read number
  string ">"
  pats <- many pPattern
  string "->"
  body <- pExpr
  return (tag, pats, body)

pLam :: Parser JamExpr
pLam = do
  string "\\"
  boundVars <- some pPattern
  string "."
  body      <- pExpr
  return $ ELam boundVars body

pAp :: Parser JamExpr
pAp = do
  fn   <- pAExpr
  args <- some pAExpr
  return $ mkApChain (flip EAp) fn args

pAExpr :: Parser JamExpr
pAExpr = pParenExpr
     <|> pPattern

pParenExpr :: Parser JamExpr
pParenExpr = do
  string "("
  expr <- pExpr
  string ")"
  return expr

pPattern :: Parser JamExpr
pPattern = pNum
       <|> pVar
       <|> pListSugar
       <|> pTupleSugar
       <|> do constr@(EConstr tag arity) <- pConstr
              args <- many pExpr
              if arity /= length args
                 then failure
                 else return $ mkApChain EAp constr args

pNum :: Parser JamExpr
pNum = fmap (ENum . read) number

pVar :: Parser JamExpr
pVar = fmap EVar variable

pConstr :: Parser JamExpr
pConstr = do
  keyword -- 'Pack'
  string "{"
  tag   <- fmap read number
  string ","
  arity <- fmap read number
  string "}"
  return $ EConstr tag arity

pOperator :: Parser JamExpr
pOperator = pMonadic
        <|> pDyadic

pMonadic :: Parser JamExpr
pMonadic = do
    op <- oneOf $ map getName builtins
    arg <- pAExpr
    let op' = builtins !! fromJust (findIndex (\builtin -> getArity builtin == "monadic" && op == getName builtin) builtins)
    return $ constructBuiltinAp op' arg
  where constructBuiltinAp :: Operator -> JamExpr -> JamExpr
        constructBuiltinAp op arg = case getArity op of
                                         "monadic" -> EAp (EVar $ getName op) arg
                                         _         -> error "dyadic operator applied as a monadic operator"

pDyadic :: Parser JamExpr
pDyadic = do
    (args, ops) <- pAExpr `sepBy` oneOf  (map getName builtins)
    let ops' = map (\op -> 
                      let index = fromJust $ findIndex (\builtin -> op == getName builtin) builtins
                       in builtins !! index)
                   ops
    return $ if (not . any ((== "monadic") . getArity)) ops'
                then constructBuiltinAp args ops'
                else error "monadic operator used as a dyadic operator"
  where constructBuiltinAp :: [JamExpr] -> [Operator] -> JamExpr
        constructBuiltinAp [expr] []                      = expr
        constructBuiltinAp [lhs, rhs] [op]                = EAp (EAp (EVar $ getName op) lhs) rhs
        constructBuiltinAp (a:b:c:restArg) (o1:o2:restOp) = 
          case o1 `compare` o2 of
               GT -> constructBuiltinAp (EAp (EAp (EVar $ getName o1) a) b:c:restArg) (o2:restOp)
               LT -> constructBuiltinAp (a:EAp (EAp (EVar $ getName o2) b) c:restArg) (o1:restOp)
               EQ -> case getFixity o1 `compare` getFixity o2 of
                          EQ -> case getFixity o1 of
                                     "none"  -> error "Ambiguous use of non-associative operators"
                                     "left"  -> constructBuiltinAp (EAp (EAp (EVar $ getName o1) a) b:c:restArg) (o2:restOp)
                                     "right" -> EAp (EAp (EVar $ getName o1) a) $ constructBuiltinAp (b:c:restArg) (o2:restOp)
                                     _ -> error "This shouldn't happen"
                          _ -> error "Fixities must match for ambiguous application of operators of the same precedence"
        constructBuiltinAp args ops = error (show args ++ " | " ++ show ops)

pListSugar :: Parser JamExpr
pListSugar = pListEmpty
         <|> pListFull

pListEmpty :: Parser JamExpr
pListEmpty = do
  string "["
  string "]"
  return $ EConstr 1 0

pListFull :: Parser JamExpr
pListFull = do
  string "["
  first <- pExpr
  rest  <- option (some (do {string ","; pAExpr})) (return [])
  string "]"
  return $ mkList (first:rest)

pTupleSugar :: Parser JamExpr
pTupleSugar = do
  string "("
  first <- pExpr
  string ","
  rest  <- pExpr `sep1` string ","
  string ")"
  return $ mkTuple (first:rest)

--------------------------------------------------------------------------------
scdefToLam :: JamScDefn -> (JamExpr, JamExpr)
scdefToLam (name, pats, body) = (EVar name, ELam pats body)

mkList :: [JamExpr] -> JamExpr
mkList = mkApChain (\outer inner -> EAp (EAp (EConstr 2 2) inner) outer) (EConstr 1 0)

mkTuple :: [JamExpr] -> JamExpr
mkTuple contents = mkApChain (flip EAp) (EConstr 1 $ length contents) contents
