module Jam.Template.Evaluator.Print
  (
    showResults,
    showResults'
  ) where

import Jam.Library.Print
import Jam.Template.Util.Compiler
import Jam.Util

showResults states = iDisplay (iConcat [iLayn (map showState states),
                                        showStats (last states)])

showResults' state@(stack, dump, heap, env, stats) 
  = iDisplay (iConcat [showStack heap stack,
                       Newline,
                       String (replicate 80 '~'),
                       showStats state])

showStats :: TiState -> ISeq
showStats (stack, dump, heap, globals, stats) = iConcat [
                                                          Newline,
                                                          Newline,
                                                          String "Total number of steps = ",
                                                          iNum (tiStatGetSteps stats)
                                                        ]

showState :: TiState -> ISeq
showState (stack, dump, heap, globals, states) = showStack heap stack <> Newline
                                              <> Newline <> dumpHeap heap <> Newline
                                              <> Newline <> dumpDump heap dump <> Newline
                                              <> String (replicate 80 '~')

dumpHeap :: TiHeap -> ISeq
dumpHeap (size, free, env) = iConcat [
                                       String $ "Heap(" ++ show size ++ ") [",
                                       Indent (Newline ~> (map showHeapItem env)),
                                       String "]"
                                     ]
  where showHeapItem :: (Addr, Node) -> ISeq
        showHeapItem (addr, node) = showFWAddr addr <> String ": " <> showNode node

dumpDump :: TiHeap -> TiDump -> ISeq
dumpDump heap stacks = iConcat [
                                 String $ "Dump(" ++ show (length stacks) ++ ") [",
                                 Indent ((Newline <> Newline) ~> (map (showStack heap) stacks)),
                                 String "]"
                               ]

showStack :: TiHeap -> TiStack -> ISeq
showStack heap stack = iConcat [
                                 String "Stack [",
                                 Indent (Newline ~> (map showStackItem stack)),
                                 String "]"
                               ]
  where showStackItem addr = iConcat [
                                       showFWAddr addr,
                                       String ": ",
                                       showStkNode heap (hLookup heap addr)
                                     ]
showStkNode :: TiHeap -> Node -> ISeq
showStkNode heap (NAp funAddr argAddr) = iConcat [
                                                   String "NAp ",
                                                   showFWAddr funAddr,
                                                   String " ",
                                                   showFWAddr argAddr,
                                                   String " (",
                                                   showNode (hLookup heap argAddr),
                                                   String ")"
                                                 ]
showStkNode heap node = showNode node

showNode :: Node -> ISeq
showNode (NAp a1 a2) = iConcat [
                                 String "NAp ",
                                 showAddr a1,
                                 String " ",
                                 showAddr a2
                               ]
showNode (NSupercomb name args body) = String ("NSupercomb " ++ name)
showNode (NNum n) = String "NNum " <> iNum n
showNode (NInd a) = String "NInd " <> showAddr a
showNode (NPrim n p) = String $ "NPrim " ++ n
showNode (NData t d) = String ("NData " ++ show t ++ " ") <> iConcat [
                                                                       String "[",
                                                                       String ", " ~> map showAddr d,
                                                                       String "]"
                                                                     ]

showAddr :: Addr -> ISeq
showAddr addr = String (show addr)

showFWAddr :: Addr -> ISeq -- Show address in field of width 4
showFWAddr addr = String (spaces (4 - length str) ++ str)
  where str = show addr
