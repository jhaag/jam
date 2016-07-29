module Jam.Library.Prelude where

import Jam.Language
import Jam.Util.Core

data Primitive = Neg | Add | Sub | Mul | Div
primitives :: Assoc Name Primitive
primitives = [("negate", Neg),
              ("+", Add), ("-", Sub),
              ("*", Mul), ("/", Div)]

preludeDefs :: CoreProgram
preludeDefs = [("I",        ["x"],            EVar "x"), 
               ("K",        ["x", "y"],       EVar "x"),
               ("K1",       ["x", "y"],       EVar "y"),
               ("S",        ["f", "g", "x"],  EAp (EAp (EVar "f") 
                                                       (EVar "x"))
                                                  (EAp (EVar "g") 
                                                       (EVar "x"))),
               ("compose",  ["f", "g", "x"],  EAp (EVar "f")
                                                  (EAp (EVar "g") 
                                                       (EVar "x"))),
               ("twice",    ["f"],            EAp (EAp (EVar "compose") 
                                                       (EVar "f")) 
                                                  (EVar "f"))]

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []
