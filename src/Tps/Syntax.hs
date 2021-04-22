{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Tps.Syntax where

import Control.Monad

import Option
import Vec

import Types (Val)

type Sig = Nat -> Bool -> *

data Tps (sig :: Sig) a where
  Leaf :: a -> Tps sig a
  Node :: sig n b
       -> Vec n (Tps sig Val)
       -> Option b (String, Tps sig a)
       -> Tps sig a

instance Monad (Tps sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ (x,k) -> (x, k >>= g)) k)

instance Functor (Tps sig) where
  fmap = liftM

instance Applicative (Tps sig) where
  pure = Leaf
  (<*>) = ap

liftT :: sig n 'True -> Vec n (Tps sig Val) -> String -> Tps sig a -> Tps sig a
liftT op ps x k = Node op ps (Some (x, k))

liftF :: sig n 'False -> Vec n (Tps sig Val) -> Tps sig a
liftF op ps = Node op ps None
