{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Tree.Syntax where

import Control.Monad

import Vec
import Option

type Sig = Nat -> Bool -> * -> * -> * -> *

data Tree (sig :: Sig) a where
  Leaf :: a -> Tree sig a
  Node :: sig n b p r q ->
          Vec n (p -> Tree sig r) ->
          Option b (q -> Tree sig a) ->
          Tree sig a

instance Monad (Tree sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ k x -> k x >>= g) k)

instance Functor (Tree sig) where
  fmap = liftM

instance Applicative (Tree sig) where
  pure = Leaf
  (<*>) = ap
  
liftT :: sig n 'True p r a -> Vec n (p -> Tree sig r) -> Tree sig a
liftT op ps = Node op ps (Some Leaf)

liftF :: sig n 'False p r q -> Vec n (p -> Tree sig r) -> Tree sig a
liftF op ps = Node op ps None

