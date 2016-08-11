module Jam.Parser.Core where

import Control.Applicative
import Control.Monad
import Jam.Language.Jam
import Jam.Util.Parser

---------------------------------------------------------- Parser Definition ---
newtype Parser a = Parser { parse :: [Token] -> [(a, [Token])] }

instance Functor Parser where
  fmap f (Parser p) = Parser (\ts -> [(f a, ts') | (a, ts') <- p ts])

instance Applicative Parser where
  pure                        = return
  (Parser p1) <*> (Parser p2) = Parser (\ts -> [(f a, ts'') | (f, ts')  <- p1 ts,
                                                              (a, ts'') <- p2 ts'])

instance Monad Parser where
  return  = unit
  (>>=)   = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

item :: Parser String
item = Parser $ \ts ->
  case ts of
       [] -> []
       ((_, s):ts) -> [(s, ts)]

unit :: a -> Parser a
unit a = Parser (\ts -> [(a, ts)])

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \ts -> concatMap (\(a, ts') -> parse (f a) ts') $ parse p ts

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\ts -> parse p ts ++ parse q ts)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \ts ->
  case parse p ts of
       []   -> parse q ts
       res  -> res

runParse :: Parser a -> [Token] -> a
runParse p ts =
  case parse p ts of
       [(res, [])] -> res
       [(r, toks)] -> r--error "Parser did not consume entire stream."
       _           -> error "Parser error."

----------------------------------------------------------- Parser Functions ---
satisfy :: (String -> Bool) -> Parser String
satisfy p = item `bind` \s ->
  if p s
     then unit s
     else failure

string :: String -> Parser String
string s = satisfy (== s)

number :: Parser String
number = satisfy (isDigit . head)

variable :: Parser String
variable = satisfy (\s -> ((\c -> isAlpha c || c == '_') . head) s && s `notElem` keywords)

keyword :: Parser String
keyword = oneOf keywords

oneOf :: [String] -> Parser String
oneOf ss = satisfy (flip elem ss)

sep1 :: Parser a -> Parser b -> Parser [a]
p `sep1` sep = do { a <- p; rest a } <|> failure
  where rest a = (do sep
                     b <- p `sep1` sep
                     return (a:b))
                 <|> return [a]

sepBy :: Parser a -> Parser b -> Parser ([a], [b])
p `sepBy` sep = do { a <- p; rest a } <|> failure
  where rest a = (do separator <- sep
                     (as, seps)  <- p `sepBy` sep
                     return (a:as, separator:seps)
                 <|> return ([a], []))

exactly :: Int -> Parser a -> Parser [a]
exactly n p
  | n <= 0    = return []
  | otherwise = sequence (replicate n p)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p sep a = (p `chainl1` sep) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` sep = do { a <- p; rest a }
  where rest a = (do f <- sep
                     b <- p
                     rest (f a b))
                 <|> return a
