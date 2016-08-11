module Jam.Language.Jam.Operators.Core
  (
    builtins,
    OpName,
    OpArity,
    OpFixity,
    OpPrecedence,
    Operator(..)
  ) where

type OpName       = String
type OpArity      = String
type OpFixity     = String
type OpPrecedence = Int
data Operator     = Operator {
                               getName       :: OpName,
                               getArity      :: OpArity,
                               getFixity     :: OpFixity,
                               getPrecedence :: OpPrecedence
                             }

instance Eq Operator where
  op1 == op2 = getName op1 == getName op2

instance Ord Operator where
  op1 `compare` op2 = getPrecedence op1 `compare` getPrecedence op2

instance Show Operator where
  show op = (show $ getName op) ++ ":" 
         ++ (show $ getArity op) ++ ":" 
         ++ (show $ getFixity op) ++ ":"
         ++ (show $ getPrecedence op)

builtins :: [Operator]
builtins = map (\(name, arity, fixity, precedence) -> Operator name arity fixity precedence) opDefs

opDefs :: [(OpName, OpArity, OpFixity, OpPrecedence)]
opDefs = [("^", "dyadic", "right", 8),
          ("*", "dyadic", "left", 7),
          ("/", "dyadic", "left", 7),
          ("+", "dyadic", "left", 6),
          ("-", "dyadic", "left", 6),
          ("-", "monadic", "none", 6),
          (":", "dyadic", "right", 5),
          ("==", "dyadic", "none", 4),
          ("/=", "dyadic", "none", 4),
          (">", "dyadic", "none", 4),
          (">=", "dyadic", "none", 4),
          ("<", "dyadic", "none", 4),
          ("<=", "dyadic", "none", 4),
          ("&&", "dyadic", "right", 3),
          ("||", "dyadic", "right", 2)]
