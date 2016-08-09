module Jam.Language.Lambda.Simple.Core where

-- expressions in the Simple Lambda Calculus
data Expr = ENum      -- Constants (only numbers for now)
              Int       -- since there are only integral constants, just Int
          | EVar      -- Variable Names
              String    -- represented by their name
          | EAp       -- Function Applications
              Expr      -- the expression that reduces to the function
              Expr      -- the expression that reduces to the argument
          | ELam      -- Lambda abstractions
              Expr      -- argument
              Expr      -- body
          deriving (Eq, Show)
