module Jam.Util.PrettyPrinter
  (
    ISeq (..),
    (~>),
    (<>),
    flatten,
    iDisplay,
    iFWNum,
    iLayn,
    iNum,
    spaces
  ) where

import Data.Monoid ((<>))

data ISeq = Empty 
          | String String
          | Append ISeq ISeq
          | Indent ISeq
          | Newline

instance Monoid ISeq where
  mappend = Append
  mempty  = Empty

(~>) :: ISeq -> [ISeq] -> ISeq
_ ~> []             = Empty
_ ~> [seq]          = seq
inter ~> (seq:seqs) = (seq <> inter) <> (inter ~> seqs)

flatten :: Int -> [(ISeq, Int)] -> String
flatten _ [] = ""
flatten col ((seq, indent):seqs) =
  case seq of
       Empty           -> flatten col seqs
       (String s)      -> s ++ (flatten (col + length s) seqs)
       (Append s1 s2)  -> flatten col ((s1, indent):(s2, indent):seqs)
       (Indent s)      -> (spaces 2) ++ flatten (col + 2) ((s, col + 2):seqs)
       Newline         -> '\n':(spaces indent) ++ (flatten indent seqs)

spaces :: Int -> String
spaces = flip replicate $ ' '

iNum :: Int -> ISeq
iNum n = String $ show n

iFWNum :: Int -> Int -> ISeq
iFWNum width n = String $ (spaces (width - length digits) ++ digits)
  where digits = show n

iLayn :: [ISeq] -> ISeq
iLayn seqs = mconcat (map layItem (zip [1..] seqs))
  where layItem (n, seq) = mconcat [iFWNum 2 n, String ") ", Indent seq, Newline]

iDisplay :: ISeq -> String
iDisplay seq = flatten 0 [(seq, 0)]
