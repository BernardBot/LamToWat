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

import qualified Data.Tree as D

import Control.Monad

import Interpreter (Interpretable, interp, letin, Dom(Fun))
import Val
import Types (Var,Sig,Treeable,toTree)

import qualified Types
import qualified Interpreter

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

instance (Show a, forall n b p r q. Show (sig n b p r q)) => Treeable (Tps sig a) where
  toTree (Leaf a) = D.Node (show a) []
  toTree (Node cmd Nil None) = D.Node (show cmd) []
  toTree (Node cmd Nil (Some ("",k))) = D.Node (show cmd) [toTree k]
  toTree (Node cmd Nil (Some (x,k))) = D.Node (x ++ " = " ++ show cmd) [toTree k]
  toTree (Node cmd ks (Some ("",k))) =
    D.Node (show cmd) [D.Node "ks" (map toTree (toList ks)), toTree k]

deriving instance (Show a, forall n b p r q. Show (sig n b p r q)) => Show (Tps sig a)

instance (Interpretable (Tps sig a)) => Interpretable (Types.Fix (Tps sig a)) where
  interp (fs,e) = Interpreter.fix (map (fmap interp) fs,interp e)

instance (Interpretable a, forall n b p r q. Interpretable (sig n b p r q)) =>
  Interpretable (Tps sig a) where
  interp (Leaf a) = interp a

  interp (Node cmd Nil None) = interp cmd
  interp (Node cmd Nil (Some ("",k))) = do
    interp cmd
    interp k
  interp (Node cmd Nil (Some (x,k))) = do
    cmd' <- interp cmd
    letin x cmd' (interp k)

  interp (Node cmd ks (Some ("",k))) = do -- hack to make fixes work
    Fun f <- interp cmd
    let ks' = mapV (\ k -> Fun $ \ [] -> interp k) ks -- hacky thunks
    let k' = Fun $ \ [] -> interp k
    f (k':toList ks')

  -- what to do here?
  -- interp (Node cmd ks None) = do
  --   Fun f <- interp cmd
  --   ks' <- mapM interp ks
  --   f $ toList ks'

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
