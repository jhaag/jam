module Jam.Util.Core where

type Addr = Int
showaddr :: Addr -> String
showaddr addr = '#':show addr

-------------------------------------------------------- Heap Data Structure ---
type Heap a = (Int, [Addr], [(Addr, a)])

-- Null Stuff
hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull addr = addr == hNull

-- Getters
hLookup :: Heap a -> Addr -> a
hLookup (size, free, env) addr 
    = aLookup env addr ("Can't find node " ++ showaddr addr ++ " in heap.")

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, env) = [addr | (addr, node) <- env]

hSize :: Heap a -> Int
hSize (size, free, env) = size

-- Core Heap Operations
hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), env) node = ((size + 1, free, (next, node):env), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, env) addr node = (size, free, (addr, node):remove env addr)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, env) addr = (size - 1, addr:free, remove env addr)

-- Helpers
remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] addr = error ("Attempt to update or free nonexistent address " ++ showaddr addr)
remove ((addr', node):env) addr
  | addr == addr' = env
  | otherwise     = (addr', node):remove env addr

----------------------------------------------------------- Association List ---
type Assoc a b = [(a, b)]

aEmpty :: Assoc a b
aEmpty = []

aLookup :: Eq a => Assoc a b -> a -> String -> b
aLookup [] key err = error err
aLookup ((key', val):rest) key err
  | key == key' = val
  | otherwise   = aLookup rest key err

aDomain :: Assoc a b -> [a]
aDomain assoc = [key | (key, val) <- assoc]

aRange :: Assoc a b -> [b]
aRange assoc = [val | (key, val) <- assoc]
