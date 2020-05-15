{-# LANGUAGE DeriveFunctor #-}

module Lec_5_15_20 where

import qualified Data.Map as M

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Eq, Show)


charT :: Tree Char
charT = Node 
            (Node 
                (Leaf 'a') 
                (Leaf 'b')) 
            (Node 
                (Leaf 'c') 
                (Leaf 'a'))




























type State = Int
data ST0 a = ST0C (State -> (State, a)) 
  deriving (Functor)








-- >>> charT
-- Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'a'))
--



data ST s a = STC (s -> (s, a)) 
  deriving (Functor)

get :: ST s s
get = STC (\s -> (s, s))

put :: s -> ST s ()
put s = STC (\_ -> (s, ()))

instance Monad (ST s) where
  return x = STC (\s -> (s, x))
  st >>= f = STC (\s -> let (s', x) = runState st s 
                        in runState (f x) s')

runState :: ST s a -> s -> (s, a)
runState (STC f) s = f s

evalState :: ST s a -> s -> a
evalState st s = snd (runState st s) 

charNext :: Char -> ST (M.Map Char Int) Int
charNext c = do
  m    <- get                     -- get current freq-map
  let n = M.findWithDefault 0 c m  -- current freq for c (or 0)
  put (M.insert c (n+1) m)        -- update freq for c
  return n                        -- return current as valu

type MapST a = ST (M.Map Char Int)  a 

keyHelperS :: Tree Char -> MapST (Tree (Char, Int))
keyHelperS (Leaf c) = do 
    n <- charNext c
    return (Leaf (c, n))

keyHelperS (Node l r) = do
    l' <- keyHelperS l
    r' <- keyHelperS r
    return (Node l' r')

keyLabelS :: Tree Char -> Tree (Char, Int)
keyLabelS t = evalState (keyHelperS t) M.empty 



instance Applicative ST0 where
  pure  = undefined
  (<*>) = undefined


instance Applicative (ST s) where
  pure  = undefined
  (<*>) = undefined