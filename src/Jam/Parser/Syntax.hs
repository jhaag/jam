module Jam.Parser.Syntax
  (
    syntax
  ) where

import Control.Applicative
import Control.Monad
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
pProgram = pSc `sep1` (string ";")

pSc :: Parser JamScDefn
pSc = do
  name <- variable
  args <- many pPattern
  string "="
  expr <- pExpr
  return (name, args, expr)

--pCAF :: Parser JamScDefn

pExpr :: Parser JamExpr
pExpr = pELam
    <|> pELet
    <|> pECase
    <|> pEAp
    <|> pOperator
--    <|> pAExpr

pPattern :: Parser JamExpr
pPattern = pENum
       <|> pEVar
       <|> pListSugar
       <|> pTupleSugar
       <|> do constr@(EConstr tag arity) <- pEConstr
              arguments <- exactly arity pExpr
              return $ mkApChain (EAp) constr arguments

pAExpr :: Parser JamExpr
pAExpr = pPattern
     <|> pParenExpr

pParenExpr :: Parser JamExpr
pParenExpr = do
  string "("
  expr <- pExpr
  string ")"
  return expr

pEVar :: Parser JamExpr
pEVar = do
  var <- variable
  return $ EVar var

pENum :: Parser JamExpr
pENum = do
  num <- number
  return $ ENum $ read num

pEConstr :: Parser JamExpr
pEConstr = do
  keyword         -- 'Pack'
  string "{"
  tag   <- number
  string ","
  arity <- number
  string "}"
  return $ EConstr (read tag) (read arity)

pELet :: Parser JamExpr
pELet = do
  letKey  <- keyword      -- 'let' or 'letrec'
  defs    <- pLetDef `sep1` (string ";")
  keyword                 -- 'in'
  expr    <- pExpr
  return $ ELet (if letKey == "letrec" then True else False) defs expr

pLetDef :: Parser (JamExpr, JamExpr)
pLetDef = do
  binding <- pPattern
  string "="
  body    <- pExpr
  return (binding, body)

pECase :: Parser JamExpr
pECase = do
  keyword                 -- 'case'
  caseExpr      <- pExpr
  keyword                 -- 'of'
  alternatives  <- pAlter `sep1` (string ";")
  return $ ECase caseExpr alternatives

pAlter :: Parser JamAlt
pAlter = do
  string "<"
  tag   <- number
  string ">"
  vars  <- many variable
  string "->"
  expr  <- pExpr
  return $ (read tag, vars, expr)

pELam :: Parser JamExpr
pELam = do
  string "\\"
  boundVars <- some pPattern
  string "."
  body      <- pExpr
  return $ ELam boundVars body

pEAp :: Parser JamExpr
pEAp = do
  fn <- pAExpr
  aExprs <- some pAExpr
  return $ mkApChain (EAp) fn (reverse aExprs)


pOperator :: Parser JamExpr
pOperator = pDyadic -- pMonadic if I need it in the future

pDyadic :: Parser JamExpr
pDyadic = do
  (args, ops) <- pAExpr `sepBy` (oneOf $ map getName builtins)
  let ops' = map (\op -> 
                    let index = fromJust $ findIndex (\builtin -> op == getName builtin) builtins
                     in builtins !! index)
                 ops
  return $ constructBuiltinAp args ops'
  where constructBuiltinAp :: [JamExpr] -> [Operator] -> JamExpr
        constructBuiltinAp [expr] []                      = expr
        constructBuiltinAp [lhs, rhs] [op]                = EAp (EAp (EVar $ getName op) lhs) rhs
        constructBuiltinAp (a:b:c:restArg) (o1:o2:restOp) = 
          case o1 `compare` o2 of
               GT -> constructBuiltinAp ((EAp (EAp (EVar $ getName o1) a) b):c:restArg) (o2:restOp)
               LT -> constructBuiltinAp (a:(EAp (EAp (EVar $ getName o2) b) c):restArg) (o1:restOp)
               EQ -> case getFixity o1 `compare` getFixity o2 of
                          EQ -> case getFixity o1 of
                                     "none"  -> error "Ambiguous use of non-associative operators"
                                     "left"  -> constructBuiltinAp ((EAp (EAp (EVar $ getName o1) a) b):c:restArg) (o2:restOp)
                                     "right" -> EAp (EAp (EVar $ getName o1) a) $ constructBuiltinAp (b:c:restArg) (o2:restOp)
                                     _ -> error "This shouldn't happen"
                          _ -> error "Fixities must match for ambiguous application of operators of the same precedence"
        constructBuiltinAp args ops = error (show args ++ " | " ++ show ops)

constructData :: (Int -> JamExpr) -> [JamExpr] -> JamExpr
constructData constr contents = mkApChain (EAp) (constr $ length contents) (reverse contents)

pListSugar :: Parser JamExpr
pListSugar = pListEmpty
         <|> pListNonEmpty

pListEmpty :: Parser JamExpr
pListEmpty = do
  string "["
  string "]"
  return $ EConstr 1 0

pListNonEmpty :: Parser JamExpr
pListNonEmpty = do
  string "["
  contents <- pAExpr `sep1` string ","
  string "]"
  return $ constructData (EConstr 2) contents

pTupleSugar :: Parser JamExpr
pTupleSugar = do
  string "("
  first <- pAExpr
  string ","
  rest <- pAExpr `sep1` string ","
  string ")"
  return $ constructData (EConstr 1) (first:rest)
