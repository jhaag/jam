module Jam.Language.Lambda.Enriched.Pretty
  (
    prettyEnriched
  ) where

import Jam.Language.Lambda.Enriched.Core
import Jam.Util.PrettyPrinter

instance Show Expr where
  show = prettyEnriched

prettyEnriched :: Expr -> String
prettyEnriched = iDisplay . prettyExpr

prettyExpr :: Expr -> ISeq
prettyExpr expr = case expr of
                       (ENum n)                 -> iNum n
                       (EVar v)                 -> String v
                       (EAp lhs rhs)            -> String "(" <> prettyExpr lhs <> String " " <> prettyExpr rhs <> String ")"
                       (ELam pat body)          -> String "λ" <> prettyPat pat <> String ".(" <> prettyExpr body <> String ")"
                       (ELet pat def expr)      -> mconcat [
                                                             String "let", 
                                                             Indent (prettyPatSepExpr " = "pat def), 
                                                             Newline,
                                                             String "in", 
                                                             Indent (prettyExpr expr)
                                                           ]
                       (ELetRec pats defs expr) -> mconcat [
                                                             String "letrec", 
                                                             Indent (Newline ~> zipWith (prettyPatSepExpr " = ") pats defs),
                                                             Newline,
                                                             String "in",
                                                             Indent (prettyExpr expr)
                                                           ]
                       bar@EFatBar{}            -> String "(" <> String " ❚ " ~> map prettyExpr (expandBar bar) <> String ")"
                       (ECase expr pats bodies) -> mconcat [
                                                             String "case ",
                                                             prettyExpr expr,
                                                             String " of",
                                                             Newline,
                                                             Indent (Newline ~> zipWith (prettyPatSepExpr " -> ") pats bodies)
                                                           ]
                       FAIL                     -> String "Ƒ"
                       ERROR                    -> String "⊥"

prettyPat :: Pat -> ISeq
prettyPat pat = case pat of
                     (PNum n)            -> iNum n
                     (PVar v)            -> String v
                     (PConstr tag pats)  -> mconcat [
                                                      String "<",
                                                      iNum tag,
                                                      String ">",
                                                      Indent (String ") " ~> map (\pat -> String "(" <> prettyPat pat) pats),
                                                      String ")"
                                                    ]

prettyPatSepExpr :: String -> Pat ->  Expr  -> ISeq
prettyPatSepExpr sep pat expr = prettyPat pat <> String sep <> prettyExpr expr

expandBar :: Expr -> [Expr]
expandBar (EFatBar lhs rhs) = lhs:expandBar rhs
expandBar expr = [expr]

-- in insert mode, press <Ctrl-v> u xxxx
-- ❚  | Fat Bar         (275a)
-- λ  | Lambda          (03bb)
-- Ƒ  | Fail            (0191)
-- ⊥  | Bottom (error)  (22a5)
