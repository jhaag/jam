module Jam.Util.Helpers where

genFreeVars :: Int -> [String]
genFreeVars 0 = []
genFreeVars n = (genFreeVars (n - 1)) ++ ["__" ++ replicate n '_']

mkApChain :: (a -> b -> a) -> a -> [b] -> a
mkApChain _ core [] = core
mkApChain constructor core (chain:rest) = constructor (mkApChain constructor core rest) chain
