module Jam.Language.Lambda.Enriched.Core where

-- Expressions in the Enriched Lambda Calculus
data Expr = ENum      -- Constants (only numbers for now)
              Int       -- int since it is only numbers for now
          | EVar      -- Variables
              String    -- indexed by string
          | EAp       -- Function applications
              Expr      -- lhs
              Expr      -- rhs
          | ELam      -- Lambda abstractions
              Pat       -- pattern for bound variable
              Expr      -- body of the lambda
          | ELet      -- Non-recursive let expressions
              Pat       -- pattern for the definition
              Expr      -- body for the definition
              Expr      -- expression to be evaluated
          | ELetRec   -- Recursive let expressions
              [Pat]     -- list of patterns for the definitions
              [Expr]    -- list of bodies for the definitions
              Expr      -- expression to be evaluated
          | EFatBar   -- Fat Bar: primary `fatbar` _ = primary; fail `fatbar` alternative = alternative
              Expr    -- primary
              Expr    -- alternative
          | ECase     -- Case expressions
              Expr      -- variable to inspect
              [Pat]     -- list of patterns for the cases
              [Expr]    -- list of bodies for the cases
          | FAIL      -- Only for use with the Fat Bar; can not appear in any other (sub-)expressions.
          deriving (Eq, Show)

-- Patterns in the Enriched Lambda Calculus
data Pat = PNum
            Int
         | PVar
            String
         | PConstr
            String -- this says <constructor> on page 40, but I'm not sure what that means yet
            [Pat]
         deriving (Eq, Show)
