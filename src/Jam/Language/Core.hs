module Jam.Language.Core where

keywords :: [String]
keywords = ["case", "in", "let", "letrec", "of", "Pack"]

operators :: [String]
operators = op0 ++ op1 ++ op2 ++ op3 ++ op4

op0, op1, op2, op3, op4 :: [String]
op0 = ["||"]
op1 = ["&&"]
op2 = ["<", "<=", "==", "/=", ">=", ">"]
op3 = ["+", "-"]
op4 = ["*", "/"]

operatorSymbols :: String
operatorSymbols = "~!@#$%^&*-+=|<>?/\\"

-- Type Synonyms
type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive     = True
nonRecursive  = False

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type CoreExpr = Expr Name
data Expr a = EVar Name             -- Variables
            | ENum Int              -- Numbers
            | EConstr Int Int       -- Constructor tag arity
            | EAp (Expr a) (Expr a) -- Applications
            | ELet                  -- Let(rec) expressions
                IsRec               --    boolean with True = recursive
                [(a, Expr a)]       --    Definitions
                (Expr a)            --    Body of let(rec)
            | ECase                 -- Case expressions
                (Expr a)            --    Expression to scrutinise
                [Alter a]           --    Alternatives
            | ELam [a] (Expr a)     -- Lambda abstractions
            deriving (Show)

data PartialExpr = NoOp
                 | FoundOp Name CoreExpr
                 deriving (Show)

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Program a = [ScDefn a]
type CoreProgram = Program Name
