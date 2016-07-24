module Jam.Library.Print where
--  (
--    prettyExpr,
--    prettyProg
--  ) where

import Jam.Language

prettyProg :: CoreProgram -> String
prettyProg = iDisplay . pprProgram

prettyExpr :: CoreExpr -> String
prettyExpr = iDisplay . pprExpr
pprProgram :: CoreProgram -> ISeq

pprProgram [] = Empty
pprProgram (sc:scs) = (pprScDefn sc) <> (pprProgram scs)

pprScDefn :: CoreScDefn -> ISeq
pprScDefn (name, args, body) = (String name) <> (String " ") <>
                               ((String " ") ~> (map String args)) <> 
                               (String " = ") <> pprExpr body <>
                               Newline

pprExpr :: CoreExpr -> ISeq
pprExpr (EVar v)                = String v
pprExpr (ENum n)                = String $ show n
pprExpr (EConstr tag arity)     = String $ "Pack{"    ++ 
                                           show tag   ++ 
                                           ","        ++ 
                                           show arity ++ 
                                           "}"
pprExpr (EAp e1 e2)             = (pprExpr e1) <> (String " ") <> (pprAExpr e2)
pprExpr (ELet isrec defns expr) = iConcat [String keyword, Newline,
                                           Indent (pprDefns defns), Newline,
                                           String "in ", pprExpr expr]
  where keyword = if isrec then "letrec" else "let"
pprExpr (ECase e alts)          = iConcat [String "case ", pprExpr e, String " of",
                                           Newline, Indent (Newline ~> (map pprAlter alts))]
pprExpr (ELam vars e)           = iConcat [String "\\", (String " ") ~> (map String vars),
                                           String " . ", pprExpr e]

pprAExpr :: CoreExpr -> ISeq
pprAExpr e
  | isAtomicExpr e  = pprExpr e
  | otherwise       = (String "(") <> pprExpr e <> (String ")")

pprDefns :: [(Name, CoreExpr)] -> ISeq
pprDefns defns = sep ~> (map pprDefn defns)
  where sep = (String ";") <> Newline

pprDefn :: (Name, CoreExpr) -> ISeq
pprDefn (name, expr) = iConcat [String name, String " = ", pprExpr expr]

pprAlter :: CoreAlt -> ISeq
pprAlter (tag, vars, e) = iConcat [String "<", String $ show tag, String ">",
                                   (if null vars then Empty else String " "),
                                   (String " ") ~> (map String vars),
                                   (String " -> "), pprExpr e]
--------------------------------------------------------------------------------
data ISeq = Empty 
          | String String
          | Append ISeq ISeq
          | Indent ISeq
          | Newline

-- Functions on ISeq
(<>) :: ISeq -> ISeq -> ISeq
Empty <> b = b
a <> Empty = a
a <> b     = Append a b

iConcat :: [ISeq] -> ISeq
iConcat []         = Empty
iConcat (seq:seqs) = seq <> (iConcat seqs)

(~>) :: ISeq -> [ISeq] -> ISeq
_ ~> []             = Empty
_ ~> [seq]          = seq
inter ~> (seq:seqs) = (seq <> inter) <> (inter ~> seqs)

flatten :: Int -> [(ISeq, Int)] -> String
flatten _ [] = ""
flatten col ((seq, indent):seqs) =
  case seq of
       Empty           -> flatten col seqs
       (String s)      -> s ++ (flatten (col + length s) seqs)
       (Append s1 s2)  -> flatten col ((s1, indent):(s2, indent):seqs)
       (Indent s)      -> (spaces 2) ++ flatten (col + 2) ((s, col + 2):seqs)
       Newline         -> '\n':(spaces indent) ++ (flatten indent seqs)

spaces :: Int -> String
spaces = flip replicate $ ' '

iNum :: Int -> ISeq
iNum n = String $ show n

iFWNum :: Int -> Int -> ISeq
iFWNum width n = String $ (spaces (width - length digits) ++ digits)
  where digits = show n

iLayn :: [ISeq] -> ISeq
iLayn seqs = iConcat (map layItem (zip [1..] seqs))
  where layItem (n, seq) = iConcat [iFWNum 2 n, String ") ", Indent seq, Newline]

iDisplay :: ISeq -> String
iDisplay seq = flatten 0 [(seq, 0)]
