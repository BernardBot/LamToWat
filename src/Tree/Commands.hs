{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Tree.Commands where

import Data.Void

import Option
import Vec

import Types (Val,Var)

import Tree.Syntax
import Tree.Union

data Base :: Sig where
  Add   :: Val -> Val ->         Base Z     True  Void  Void Val
  App   :: Val -> [Val] ->       Base Z     False Void  Void Val

data Fix :: Sig where
  Fix   :: Vec n (Var, [Var]) -> Fix  n     True  ()    Val  ()

data Comp :: Sig where
  GetK  :: String ->             Comp Z     True  Val   Val  Val
  SetK  :: String -> Val ->      Comp Z     True  Val   Val  ()
  Block ::                       Comp (S Z) True  ()    Val  Val
  Fresh :: String ->             Comp Z     True  Void  Void String

----------------------------
--- boilerplate liftings ---
----------------------------

add :: Base :<: sig => Val -> Val -> Tree sig Val
add v1 v2 = liftT (inj (Add v1 v2)) Nil
app :: Base :<: sig => Val -> [Val] -> Tree sig a
app v vs = liftF (inj (App v vs)) Nil

fix :: Fix :<: sig => Vec n (Var, [Var], Tree sig Val) -> Tree sig ()
fix fs =
  let VPair (fxs, bs) =
        ifoldV (VPair (Nil, Nil))
               (\ (f, xs, b) (VPair (fxs, bs)) ->
                  VPair ((f, xs) ::: fxs, b ::: bs))
               fs
  in liftT (inj (Fix fxs)) (mapV const bs)
data VPair a b n = VPair { getVPair :: (Vec n a, Vec n b) }

setk :: Comp :<: sig => Var -> Val -> Tree sig ()
setk x v = liftT (inj (SetK x v)) Nil
getk :: Comp :<: sig => Var -> Tree sig Val
getk x = liftT (inj (GetK x)) Nil
block :: Comp :<: sig => Tree sig Val -> Tree sig Val
block b = liftT (inj Block) (const b ::: Nil)
fresh :: Comp :<: sig => String -> Tree sig String
fresh v = liftT (inj (Fresh v)) Nil
