{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tps where

import Val
import Option
import Vec

import Control.Monad

import Data.Void

data Tps sig a where
  Leaf :: a -> Tps sig a
  Node :: sig n b c
       -> Vec n (Tps sig Val)
       -> Option b (Option c String, Tps sig a)
       -> Tps sig a

instance Monad (Tps sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ (x,k) -> (x, k >>= g)) k)

instance Functor (Tps sig) where
  fmap = liftM

instance Applicative (Tps sig) where
  pure = Leaf
  (<*>) = ap

liftT op ps x k = Node op ps (Some (x, k))

liftF :: sig n False c -> Vec n (Tps sig Val) -> Tps sig a
liftF op ps = Node op ps None

------------------
-- CPS Commands --
------------------

type Sig = Nat -> Bool -> Bool -> *

data Base :: Sig where
  App :: Val -> [Val] -> Base Z False False
  Add :: Val -> Val ->   Base Z True  True

data Record :: Sig where
  Record :: [Val] ->      Record Z True True
  Select :: Int -> Val -> Record Z True True

data Fix :: Sig where
  Fix :: Vec n (String,[String]) -> Fix n True False

data Malloc :: Sig where
  Malloc :: Int ->               Malloc Z True True
  Load   :: Int -> Val ->        Malloc Z True True
  Store  :: Int -> Val -> Val -> Malloc Z True False

data VoidCmd :: Sig where

----------------------------
-- Injection / Projection --
----------------------------

data (:+:) :: Sig -> Sig -> Sig where
  L :: sigl n b c -> (sigl :+: sigr) n b c
  R :: sigr n b c -> (sigl :+: sigr) n b c
infixr 7 :+:

infixr 6 :<:
class (sub :: Sig) :<: (sup :: Sig) where
  inj :: sub n b c -> sup n b c

instance a :<: a where
  inj = id

instance a :<: a :+: b where
  inj = L

instance {-# OVERLAPPABLE #-} a :<: c => a :<: b :+: c where
  inj = R . inj

------------------------
-- Smart Constructors --
------------------------
done = Leaf
app v vs = liftF (inj (App v vs)) Nil

add v1 v2 x k = liftT (inj (Add v1 v2)) Nil (Some x) k
add_ v1 v2 x = add v1 v2 x (Leaf ())

record vs x k = liftT (inj (Record vs)) Nil (Some x) k
record_ vs x = record vs x (Leaf ())

select i v x k = liftT (inj (Select i v)) Nil (Some x) k
select_ i v x = select i v x (Leaf ())

fix fxs bs k = liftT (inj (Fix fxs)) bs None k
fix_ fxs bs = fix fxs bs (Leaf ())

malloc i x k = liftT (inj (Malloc i)) Nil (Some x) k
malloc_ i x = malloc i x (Leaf ())

load i v x k = liftT (inj (Load i v)) Nil (Some x) k
load_ i v x = load i v x (Leaf ())

store i s t k = liftT (inj (Store i s t)) Nil None k
store_ i s t = store i s t (Leaf ())

----------------------
-- Helper Functions --
----------------------

liftSigF :: (forall n b c. sig n b c -> sig' n b c) -> Tps sig Val -> Tps sig' Val
liftSigF f tree = go tree
  where go (Leaf v) = Leaf v
        go (Node cmd ks k) = Node (f cmd) (fmap go ks) (fmap (fmap go) k)
                       
swap' :: (f :+: g :+: h) n b c -> (g :+: f :+: h) n b c
swap' (L a)     = R (L a)
swap' (R (L a)) = L a
swap' (R (R a)) = R (R a)

swap :: Tps (f :+: g :+: h) Val -> Tps (g :+: f :+: h) Val
swap = liftSigF swap'
