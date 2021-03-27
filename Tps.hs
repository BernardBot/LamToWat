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
       -> Option b (Option c String, Tps sig a) -- maybe just String instead of Option c String?
       -> Tps sig a

instance Monad (Tps sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ (x,k) -> (x, k >>= g)) k)

instance Functor (Tps sig) where
  fmap = liftM

instance Applicative (Tps sig) where
  pure = Leaf
  (<*>) = ap

liftT :: sig n True True -> Vec n (Tps sig Val) -> String -> Tps sig Val
liftT op ps x = Node op ps (Some (Some x, Leaf (VAR x))) -- big wrap

liftT' :: sig n True False -> Vec n (Tps sig Val) -> Tps sig ()
liftT' op ps = Node op ps (Some (None, Leaf ()))

liftF op ps   = Node op ps None

------------------
-- CPS Commands --
------------------

type Sig = Nat -> Bool -> Bool -> *

data Base :: Sig where
  App :: Val -> Val -> Base Z False False
  Add :: Val -> Val -> Base Z True True

data Record :: Sig where
  Record :: [Val] ->      Record Z True True
  Select :: Int -> Val -> Record Z True True

data Fix :: Sig where
  Fix :: Vec n (String,[String]) -> Fix n True False

data Malloc :: Sig where
  Malloc :: Int ->               Malloc Z True True
  Load   :: Int -> Val ->        Malloc Z True True
  Store  :: Int -> Val -> Val -> Malloc Z True False

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
  prj :: sup n b c -> Maybe (sub n b c)

instance a :<: a where
  inj = id
  prj = Just

instance a :<: a :+: b where
  inj = L
  prj (L a) = Just a
  prj _ = Nothing

instance {-# OVERLAPPABLE #-} a :<: c => a :<: b :+: c where
  inj = R . inj
  prj (R bc) = prj bc
  prj _ = Nothing

------------------------
-- Smart Constructors --
------------------------

app v vs = liftF (inj (App v vs)) Nil
add v1 v2 x = liftT (inj (Add v1 v2)) Nil x

record vs x = liftT (inj (Record vs)) Nil x
select i v x = liftT (inj (Select i v)) Nil x

data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }

fix fs =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT' (inj (Fix fxs)) bs

fix' fxs bs = liftT' (inj (Fix fxs)) bs

malloc i x = liftT (inj (Malloc i)) Nil x
load i v x = liftT (inj (Load i v)) Nil x
store i s t = liftT' (inj (Store i s t)) Nil
