module Jam.Language.Jam.Core where

import Jam.Language.Jam.Operators

keywords :: [String]
keywords = ["case", "in", "let", "letrec", "of", "Pack"]

operatorSymbols :: String
operatorSymbols = "~!@#$%^&*-+=|<>?/\\"

-- Type Synonyms
type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive     = True
nonRecursive  = False

type Alter a = (Int, [a], Expr a)
type JamAlt = Alter Name

type JamExpr = Expr Name
data Expr a = EVar Name             -- Variables
            | ENum Int              -- Numbers
            | EConstr Int Int       -- Constructor tag arity
            | EAp (Expr a) (Expr a) -- Applications
            | ELet                  -- Let(rec) expressions
                IsRec               --    boolean with True = recursive
                [(Expr a, Expr a)]       --    Definitions
                (Expr a)            --    Body of let(rec)
            | ECase                 -- Case expressions
                (Expr a)            --    Expression to scrutinise
                [Alter a]           --    Alternatives
            | ELam [Expr a] (Expr a)     -- Lambda abstractions
            deriving (Eq, Show)

data PartialExpr = NoOp
                 | FoundOp Operator JamExpr
                 deriving (Show)

type ScDefn a = (Name, [Expr a], Expr a)
type JamScDefn = ScDefn Name

type Program a = [ScDefn a]
type JamProgram = Program Name
