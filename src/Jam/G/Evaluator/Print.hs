module Jam.G.Evaluator.Print where

import Jam.G.Util.Compiler
import Jam.Language
import Jam.Library.Print
import Jam.Util.Core

showResults :: [GmState] -> String
showResults states = iDisplay (iConcat [
                                         String "Supercombinator definitions:", Newline,
                                         Newline ~> map (showSc s) (getEnv s),
                                         Newline, String (replicate 80 '~'), Newline,
                                         String "State transitions:", Newline,
                                         iLayn (map showState states),
                                         Newline, String (replicate 80 '~'), Newline,
                                         showStats (last states)
                                       ])
  where (s:ss) = states

-- For pretty printing supercombinator definitions
showSc :: GmState -> (Name, Addr) -> ISeq
showSc s (name, addr) = iConcat [
                                  String ("Code for: " ++ name), Newline,
                                  showInstructions code, Newline, Newline
                                ]
  where (NGlobal arity code) = hLookup (getHeap s) addr

showInstructions :: GmCode -> ISeq
showInstructions is = iConcat [
                                String "  Code:{",
                                Indent (Newline ~> map showInstruction is),
                                String "}", Newline
                              ]

showInstruction :: Instruction -> ISeq
showInstruction Unwind         = String "Unwind"
showInstruction (Pushglobal f) = String $ "Pushglobal " ++ f
showInstruction (Push n)       = String $ "Push " ++ show n
showInstruction (Pushint n)    = String $ "Pushint " ++ show n
showInstruction Mkap           = String "Mkap"
showInstruction (Update n)     = String $ "Update " ++ show n
showInstruction (Pop n)        = String $ "Pop " ++ show n

-- For printing the state transitions from execution
showState :: GmState -> ISeq
showState s = iConcat [
                        showStack s, Newline,
                        showInstructions (getCode s), Newline
                      ]

showStack :: GmState -> ISeq
showStack s = iConcat [
                        String " Stack:[",
                        Indent (Newline ~> map (showStackItem s) (reverse (getStack s))),
                        String "]"
                      ]

showStackItem :: GmState -> Addr -> ISeq
showStackItem s a = iConcat [
                              String (showAddr a), String ": ",
                              showNode s a (hLookup (getHeap s) a)
                            ]

showNode :: GmState -> Addr -> Node -> ISeq
showNode s a (NNum n) = iNum n
showNode s a (NGlobal n g) = String $ "Global " ++ v
  where v = head [n | (n, b) <- getEnv s, a == b]
showNode s a (NAp a1 a2) = String $ "Ap " ++ showAddr a1 ++ " " ++ showAddr a2
showNode s a (NInd a') = String $ "Indirection " ++ showAddr a'

-- For printing statistics about the compilation and execution process
showStats :: GmState -> ISeq
showStats s = String "Steps taken = " <> iNum (statGetSteps (getStats s))

-- Helpers
showAddr :: Addr -> String
showAddr = show
