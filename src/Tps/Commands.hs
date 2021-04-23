{-# LANGUAGE FlexibleContexts #-}

module Tps.Commands where

import Types (Val,Var)

import Vec
import Union
import Commands

import Tps.Syntax

done = Leaf
app v vs = liftF (inj (App v vs)) Nil

add v1 v2 x k = liftT (inj (Add v1 v2)) Nil x k
add_ v1 v2 x = add v1 v2 x (Leaf ())

record vs x k = liftT (inj (Record vs)) Nil x k
record_ vs x = record vs x (Leaf ())

select i v x k = liftT (inj (Select i v)) Nil x k
select_ i v x = select i v x (Leaf ())

data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }

fix fs k =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (inj (Fix fxs)) bs "" k

fix_ fs = fix fs (Leaf ())

fix' fxs bs k = liftT (inj (Fix fxs)) bs "" k
fix'_ fxs bs = fix' fxs bs (Leaf ())

malloc i x k = liftT (inj (Malloc i)) Nil x k
malloc_ i x = malloc i x (Leaf ())

load i v x k = liftT (inj (Load i v)) Nil x k
load_ i v x = load i v x (Leaf ())

store i s t k = liftT (inj (Store i s t)) Nil "" k
store_ i s t = store i s t (Leaf ())

