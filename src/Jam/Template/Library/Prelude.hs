module Jam.Template.Library.Prelude where

import Jam.Language
import Jam.Util.Core

data Primitive = Neg
               | Add
               | Sub
               | Mul
               | Div
               | PrimConstr Int Int -- tag and arity
               | If
               | G
               | GE
               | L
               | LE
               | E
               | NE
               deriving (Eq)

primitives :: Assoc Name Primitive
primitives = [("negate", Neg),
              ("+", Add), ("-", Sub),
              ("*", Mul), ("/", Div),
              ("if", If),
              (">", G), (">=", GE),
              ("<", L), ("<=", LE),
              ("==", E), ("/=", NE)]

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []
