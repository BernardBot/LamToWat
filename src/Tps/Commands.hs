{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Tps.Commands where

import Val
import Types (Var)

import Vec
import Union
import Commands

import Tps.Syntax

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
