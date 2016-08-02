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
extraPreludeDefs = [("true", [], EConstr 2 0),
                    ("false", [], EConstr 1 0),
                    ("not", ["x"], (EAp (EAp (EVar "/=")
                                             (EVar "true"))
                                        (EVar "x"))),
                    ("&&", ["x", "y"], (EAp (EAp (EAp (EVar "if") 
                                                      (EVar "x"))
                                                 (EAp (EAp (EAp (EVar "if")
                                                                (EVar "y"))
                                                           (EVar "true"))
                                                      (EVar "false")))
                                            (EVar "false"))),
                    ("||", ["x", "y"], (EAp (EAp (EAp (EVar "if") 
                                                      (EVar "x"))
                                                 (EVar "true"))
                                            (EAp (EAp (EAp (EVar "if")
                                                           (EVar "y"))
                                                      (EVar "true"))
                                                 (EVar "false"))))]
