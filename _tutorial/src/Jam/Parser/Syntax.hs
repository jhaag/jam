module Jam.Parser.Syntax
  (
    syntax
  ) where

import Control.Applicative
import Control.Monad
import Jam.Language
import Jam.Parser.Core
import Jam.Util.Parser

--------------------------------------------------------------------------------
syntax :: [Token] -> CoreProgram
syntax = runParse pProgram

pProgram :: Parser CoreProgram
pProgram = pSc `sep1` (string ";")

pSc :: Parser CoreScDefn
pSc = do
  name <- variable
  args <- many variable
  string "="
  expr <- pExpr
  return (name, args, expr)

pExpr :: Parser CoreExpr
pExpr = pELam
    <|> pELet
    <|> pECase
    <|> pEAp
    <|> pEBin

pAExpr :: Parser CoreExpr
pAExpr = pEConstr
     <|> pParenExpr
     <|> pEVar
     <|> pENum

pParenExpr :: Parser CoreExpr
pParenExpr = do
  string "("
  expr <- pExpr
  string ")"
  return expr

pEVar :: Parser CoreExpr
pEVar = do
  var <- variable
  return $ EVar var

pENum :: Parser CoreExpr
pENum = do
  num <- number
  return $ ENum $ read num

pEConstr :: Parser CoreExpr
pEConstr = do
  keyword         -- 'Pack'
  string "{"
  tag   <- number
  string ","
  arity <- number
  string "}"
  return $ EConstr (read tag) (read arity)

pELet :: Parser CoreExpr
pELet = do
  letKey  <- keyword      -- 'let' or 'letrec'
  defs    <- pLetDef `sep1` (string ";")
  keyword                 -- 'in'
  expr    <- pExpr
  return $ ELet (if letKey == "letrec" then True else False) defs expr

pLetDef :: Parser (Name, CoreExpr)
pLetDef = do
  binding <- variable
  string "="
  body    <- pExpr
  return (binding, body)

pECase :: Parser CoreExpr
pECase = do
  keyword                 -- 'case'
  caseExpr      <- pExpr
  keyword                 -- 'of'
  alternatives  <- pAlter `sep1` (string ";")
  return $ ECase caseExpr alternatives

pAlter :: Parser CoreAlt
pAlter = do
  string "<"
  tag   <- number
  string ">"
  vars  <- many variable
  string "->"
  expr  <- pExpr
  return $ (read tag, vars, expr)

pELam :: Parser CoreExpr
pELam = do
  string "\\"
  boundVars <- some variable
  string "."
  body      <- pExpr
  return $ ELam boundVars body

pEAp :: Parser CoreExpr
pEAp = do
  fn <- pAExpr
  aExprs <- some pAExpr
  let descendLeft fn as = mkApChain fn (init as)
      mkApChain :: CoreExpr -> [CoreExpr] -> CoreExpr
      mkApChain fn [a]  = EAp fn a
      mkApChain fn as   = EAp (descendLeft fn as) (last as)
      (lExp, rExp) = case aExprs of
                          [a] -> (fn, a)
                          as  -> (descendLeft fn as, last as)
  return $ EAp lExp rExp

pEBin :: Parser CoreExpr
pEBin = pExpr0

mkAp :: CoreExpr -> PartialExpr -> CoreExpr
mkAp lexp (FoundOp op rexp) = EAp (EAp (EVar op) lexp) rexp

pExpr0 :: Parser CoreExpr
pExpr0 = do
  exp1  <- pExpr1
  pExpr <- pExpr0'
  return $ (case pExpr of
              NoOp    -> exp1
              parsed  -> mkAp exp1 parsed)
  where pExpr0' :: Parser PartialExpr
        pExpr0' = option (do
                            op  <- oneOf op0
                            exp <- pExpr0
                            return $ FoundOp op exp)
                          (return NoOp)

pExpr1 :: Parser CoreExpr
pExpr1 = do
  exp2  <- pExpr2
  pExpr <- pExpr1'
  return $ (case pExpr of
              NoOp    -> exp2
              parsed  -> mkAp exp2 parsed)
  where pExpr1' :: Parser PartialExpr
        pExpr1' = option (do
                            op  <- oneOf op1
                            exp <- pExpr1
                            return $ FoundOp op exp)
                          (return NoOp) 

pExpr2 :: Parser CoreExpr
pExpr2 = do
  exp3  <- pExpr3
  pExpr <- pExpr2'
  return $ (case pExpr of
              NoOp    -> exp3
              parsed  -> mkAp exp3 parsed)
  where pExpr2' :: Parser PartialExpr
        pExpr2' = option (do
                            op  <- oneOf op2
                            exp <- pExpr3
                            return $ FoundOp op exp)
                          (return NoOp)

pExpr3 :: Parser CoreExpr
pExpr3 = do
  exp4  <- pExpr4
  pExpr <- pExpr3'
  return $ (case pExpr of
              NoOp    -> exp4
              parsed  -> mkAp exp4 pExpr)
  where pExpr3' :: Parser PartialExpr
        pExpr3' = option (do
                            op  <- oneOf op3
                            exp <- pExpr3
                            return $ FoundOp op exp)
                          (return NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = do
  aExp  <- pAExpr
  pExpr <- pExpr4'
  return $ (case pExpr of
              NoOp    -> aExp
              parsed  -> mkAp aExp pExpr)
  where pExpr4' :: Parser PartialExpr
        pExpr4' = option (do
                            op  <- oneOf op4
                            exp <- pExpr4
                            return $ FoundOp op exp)
                          (return NoOp)
