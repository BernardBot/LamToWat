{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module CTree.Tps where

import Control.Monad

import Val
import Types (Var,Sig)

import CTree.Option
import CTree.Vec
import CTree.Union
import CTree.Commands

data Tps (sig :: Sig) a where
  Leaf :: a -> Tps sig a
  Node :: sig n b p r q ->
          Vec n (Tps sig Val) ->
          Option b (Var, Tps sig a) ->
          Tps sig a

instance Monad (Tps sig) where
  Leaf x       >>= g = g x
  Node op cs k >>= g = Node op cs (fmap (\ (x,k) -> (x, k >>= g)) k)

instance Functor (Tps sig) where
  fmap = liftM

instance Applicative (Tps sig) where
  pure = Leaf
  (<*>) = ap

liftT :: sig n 'True p r q -> Vec n (Tps sig Val) -> String -> Tps sig a -> Tps sig a
liftT op ps x k = Node op ps (Some (x, k))

liftF :: sig n 'False p r q -> Vec n (Tps sig Val) -> Tps sig a
liftF op ps = Node op ps None

deriving instance (Show a, forall n b p r q. Show (sig n b p r q)) => Show (Tps sig a)

-- Helper Function

swapTps :: Tps (l :+: r :+: t) a -> Tps (r :+: l :+: t) a
swapTps (Leaf a)                = Leaf a
swapTps (Node (L    cmd)  ks k) = Node (R (L cmd)) (fmap swapTps ks) (fmap (fmap swapTps) k)
swapTps (Node (R (L cmd)) ks k) = Node (L    cmd)  (fmap swapTps ks) (fmap (fmap swapTps) k)
swapTps (Node (R (R cmd)) ks k) = Node (R (R cmd)) (fmap swapTps ks) (fmap (fmap swapTps) k)

--------------
-- Commands --
--------------

done :: a -> Tps sig a
done = Leaf

app :: Base :<: sig => Val -> [Val] -> Tps sig a
app v vs = liftF (inj (App v vs)) Nil
add :: Base :<: sig => Val -> Val -> String -> Tps sig a -> Tps sig a
add v1 v2 x k = liftT (inj (Add v1 v2)) Nil x k
add_ :: Base :<: sig => Val -> Val -> String -> Tps sig ()
add_ v1 v2 x = add v1 v2 x (Leaf ())

record :: Record :<: sig => [Val] -> String -> Tps sig a -> Tps sig a
record vs x k = liftT (inj (Record vs)) Nil x k
record_ :: Record :<: sig => [Val] -> String -> Tps sig ()
record_ vs x = record vs x (Leaf ())
select :: Record :<: sig => Int -> Val -> String -> Tps sig a -> Tps sig a
select i v x k = liftT (inj (Select i v)) Nil x k
select_ :: Record :<: sig => Int -> Val -> String -> Tps sig ()
select_ i v x = select i v x (Leaf ())

data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }
fix :: Fix :<: sig => Vec n (Var, [Var], Tps sig Val) -> Tps sig a -> Tps sig a
fix fs k =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (inj (Fix fxs)) bs "" k
fix_ :: Fix :<: sig => Vec n (Var, [Var], Tps sig Val) -> Tps sig ()
fix_ fs = fix fs (Leaf ())
fix' :: Fix :<: sig => Vec n (Var, [Var]) -> Vec n (Tps sig Val) -> Tps sig a -> Tps sig a
fix' fxs bs k = liftT (inj (Fix fxs)) bs "" k
fix'_ :: Fix :<: sig => Vec n (Var, [Var]) -> Vec n (Tps sig Val) -> Tps sig ()
fix'_ fxs bs = fix' fxs bs (Leaf ())

malloc :: Malloc :<: sig => Int -> String -> Tps sig a -> Tps sig a
malloc i x k = liftT (inj (Malloc i)) Nil x k
malloc_ :: Malloc :<: sig => Int -> String -> Tps sig ()
malloc_ i x = malloc i x (Leaf ())
load :: Malloc :<: sig => Int -> Val -> String -> Tps sig a -> Tps sig a
load i v x k = liftT (inj (Load i v)) Nil x k
load_ :: Malloc :<: sig => Int -> Val -> String -> Tps sig ()
load_ i v x = load i v x (Leaf ())
store :: Malloc :<: sig => Int -> Val -> Val -> Tps sig a -> Tps sig a
store i s t k = liftT (inj (Store i s t)) Nil "" k
store_ :: Malloc :<: sig => Int -> Val -> Val -> Tps sig ()
store_ i s t = store i s t (Leaf ())
