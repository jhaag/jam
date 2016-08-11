module Jam.Simplify.Jam
  (
    toEnriched
  ) where

import           Control.Arrow ((***), second)
import           Data.List (groupBy, partition, sortBy)
import qualified Jam.Language.Jam             as Jam
import qualified Jam.Language.Lambda.Enriched as Enriched
import           Jam.Util.Helpers

toEnriched :: Jam.JamProgram -> Enriched.Expr
toEnriched program = let ([(_, _, mainExpr)], rest) = partition (\(name, _, _) -> name == "main") program
                         compNames = \(name1, _, _) (name2, _, _) -> name1 `compare` name2
                         rest' = (groupBy (\a b -> compNames a b == EQ)) . sortBy compNames $ rest
                         (pats, exprs) = unzip . map translateDefinition $ rest'
                      in Enriched.ELetRec pats exprs (translateExpression mainExpr)

translateDefinition :: [Jam.JamScDefn] -> (Enriched.Pat, Enriched.Expr)
translateDefinition [(name, args, expr)] = (Enriched.PVar name, mkApChain (flip (Enriched.ELam)) 
                                                                          (translateExpression expr) 
                                                                          (map translatePattern args))
translateDefinition scs                  = 
  let ((n:ames), args, exprs) = unzip3 scs
      freeArgs = genFreeVars (length $ head args)
      defs = zip exprs args
      expr' = constructCases freeArgs defs
      constructCases :: [String] -> [(Jam.JamExpr, [Jam.JamExpr])] -> Enriched.Expr
      constructCases freeVars defs = mkApChain (flip (Enriched.ELam)) 
                                               (mkApChain (Enriched.EAp) (constructFatBar defs) (map Enriched.EVar freeVars))
                                               (map (translatePattern . Jam.EVar) freeVars)
      constructFatBar :: [(Jam.JamExpr, [Jam.JamExpr])] -> Enriched.Expr
      constructFatBar defs = mkApChain (flip (Enriched.EFatBar)) 
                                       Enriched.ERROR
                                       (map (uncurry (mkApChain (flip (Enriched.ELam)))) 
                                                     (map (translateExpression *** map translatePattern) defs))
   in (Enriched.PVar n, expr')

translateExpression :: Jam.JamExpr -> Enriched.Expr
translateExpression expr = case expr of
                                Jam.EVar{}    -> translateJamVar expr
                                Jam.ENum{}    -> translateJamNum expr
                                Jam.EConstr{} -> translateJamConstr expr
                                Jam.EAp{}     -> translateJamAp expr
                                Jam.ELet{}    -> translateJamLet expr
                                Jam.ECase{}   -> translateJamCase expr
                                Jam.ELam{}    -> translateJamLam expr

translatePattern :: Jam.JamExpr -> Enriched.Pat
translatePattern expr = 
  case expr of
       (Jam.EVar v) -> Enriched.PVar v
       (Jam.ENum n) -> Enriched.PNum n
       (Jam.EConstr tag 0) -> Enriched.PConstr tag [] -- For enumeratred types
       ap@Jam.EAp{} -> let (fn, args) = unwrapAp ap
                        in case fn of
                                (Jam.EConstr tag arity) -> if length args /= arity
                                                              then error "Incomplete constructor"
                                                              else Enriched.PConstr tag (map translatePattern args)
                                _                       -> error "Patterns can only consist of variables, numbers, and constructors"

translateJamVar :: Jam.JamExpr -> Enriched.Expr
translateJamVar (Jam.EVar var) = Enriched.EVar var

translateJamNum :: Jam.JamExpr -> Enriched.Expr
translateJamNum (Jam.ENum num) = Enriched.ENum num

--TODO: Revisit when I figure out how to represent structured data as a regular expression (not a pattern)
translateJamConstr :: Jam.JamExpr -> Enriched.Expr
translateJamConstr (Jam.EConstr tag arity) = Enriched.EVar $ "{" ++ show tag ++ "," ++ show arity ++ "}"

translateJamAp :: Jam.JamExpr -> Enriched.Expr
translateJamAp (Jam.EAp expr1 expr2) = Enriched.EAp (translateExpression expr1) (translateExpression expr2)

translateJamLet :: Jam.JamExpr -> Enriched.Expr
translateJamLet (Jam.ELet recursive defs body) = 
  if recursive
     then let (pats, exprs) = (map translatePattern *** map translateExpression) $ unzip defs
           in Enriched.ELetRec pats exprs (translateExpression body)
     else if null defs
             then translateExpression body
             else let (pat, expr) = head defs
                   in Enriched.ELet (translatePattern pat) 
                                    (translateExpression expr) 
                                    (translateExpression (Jam.ELet recursive (tail defs) body))
 
translateJamCase :: Jam.JamExpr -> Enriched.Expr
translateJamCase (Jam.ECase expr alternatives) = undefined

translateAlt :: Jam.JamAlt -> Enriched.Expr
translateAlt (tag, args, body) = undefined

translateJamLam :: Jam.JamExpr -> Enriched.Expr
translateJamLam (Jam.ELam args expr) = mkApChain (flip (Enriched.ELam)) (translateExpression expr) (map translatePattern args)

-------------------------------------------------------------------- Helpers ---
unwrapAp :: Jam.JamExpr -> (Jam.JamExpr, [Jam.JamExpr])
unwrapAp (Jam.EAp (lhs@Jam.EAp{}) rhs) = second (++[rhs]) $ unwrapAp lhs
unwrapAp (Jam.EAp fn arg) = (fn, [arg])
