module Jam.G.Evaluator.Print
  (
    showResults,
    showResults'
  ) where

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

showResults' :: GmState -> String
showResults' state = iDisplay (iConcat [
                                         String "Result: ", 
                                         Indent (showState state), Newline,
                                         String (replicate 80 '~'), Newline, 
                                         showStats state
                                       ])

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

shortShowInstructions :: Int -> GmCode -> ISeq
shortShowInstructions number code = iConcat [
                                              String "{", 
                                              String "; " ~> dotcodes, 
                                              String "}"
                                            ]
  where codes = map showInstruction (take number code)
        dotcodes
          | length code > number = codes ++ [String "..."]
          | otherwise            = codes

showInstruction :: Instruction -> ISeq
showInstruction Unwind           = String "Unwind"
showInstruction (Pushglobal f)   = String $ "Pushglobal " ++ f
showInstruction (Push n)         = String $ "Push " ++ show n
showInstruction (Pushint n)      = String $ "Pushint " ++ show n
showInstruction Mkap             = String "Mkap"
showInstruction (Update n)       = String $ "Update " ++ show n
showInstruction (Pop n)          = String $ "Pop " ++ show n
showInstruction (Slide n)        = String $ "Slide " ++ show n
showInstruction (Alloc n)        = String $ "Alloc " ++ show n
showInstruction Eval             = String "Eval"
showInstruction Add              = String "+"
showInstruction Sub              = String "-"
showInstruction Mul              = String "*"
showInstruction Div              = String "/"
showInstruction Neg              = String "negate"
showInstruction Eq               = String "=="
showInstruction Ne               = String "/="
showInstruction Lt               = String "<"
showInstruction Le               = String "<="
showInstruction Gt               = String ">"
showInstruction Ge               = String ">="
showInstruction (Cond c1 c2)     = String "Cond: " <> Indent (showInstructions c1 <> Newline <> Newline <> showInstructions c2)
showInstruction (Pack tag arity) = String $ "Pack{" ++ show tag ++ "," ++ show arity ++ "}"
showInstruction (Casejump cases) = String "Cases: " <> Indent (Newline ~> map showCase cases)
showInstruction (Split n)        = String $ "Split " ++ show n
showInstruction Print            = String "Print"

showCase :: (Int, GmCode) -> ISeq
showCase (tag, code) = iConcat [
                                 String "| ", iNum tag, String " -> ",
                                 shortShowInstructions 3 code
                               ]

-- For printing the state transitions from execution
showState :: GmState -> ISeq
showState s = iConcat [
                        showOutput s, Newline,
                        showStack s, Newline,
                        showDump s, Newline,
                        showInstructions (getCode s), Newline
                      ]

showOutput :: GmState -> ISeq
showOutput s = String "Output:\"" <> String (getOutput s) <> String "\""

showDump :: GmState -> ISeq
showDump s = iConcat [
                        String "  Dump:[",
                        Indent (Newline ~> map showDumpItem (reverse (getDump s))),
                        String "]"
                      ]

showDumpItem :: GmDumpItem -> ISeq
showDumpItem (code, stack) = iConcat [
                                       String "<",
                                       shortShowInstructions 3 code, String ", ",
                                       shortShowStack stack, String ">"
                                     ]

showStack :: GmState -> ISeq
showStack s = iConcat [
                        String " Stack:[",
                        Indent (Newline ~> map (showStackItem s) (reverse (getStack s))),
                        String "]"
                      ]

shortShowStack :: GmStack -> ISeq
shortShowStack stack = iConcat [
                                 String "[",
                                 String ", " ~> map (String . showAddr) stack,
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
showNode s a (NConstr t as) = iConcat [
                                        String "Constructor ", iNum t, String " [",
                                        String ", " ~> map (String . showAddr) as,
                                        String "]"
                                      ]

-- For printing statistics about the compilation and execution process
showStats :: GmState -> ISeq
showStats s = String "Steps taken = " <> iNum (statGetSteps (getStats s))

-- Helpers
showAddr :: Addr -> String
showAddr = show
