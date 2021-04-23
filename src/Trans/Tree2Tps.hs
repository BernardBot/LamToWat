{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Trans.Tree2Tps where

import Types (Val(INT,VAR,LABEL),fresh)
import Vec
import Option
import Commands
import Union

import Tree.Syntax
import qualified Tree.Commands as T (app)

import Tps.Syntax (Tps)
import Tps.Commands

import Control.Monad.State hiding (fix)
import Control.Monad.Reader hiding (fix)

type M = StateT Int (Reader [(String,Val)])


tree2tps :: Tree (Comp :+: Fix :+: Base) Val
         -> Tps (Fix :+: Base :+: Empty) Val
tree2tps =
  fst .
  flip runReader [] .
  flip runStateT 0 .
  t2t

t2t :: Tree (Comp :+: Fix :+: Base) Val -> M (Tps (Fix :+: Base :+: Empty) Val)
t2t (Leaf x) = return (done x)
t2t (Node (R (R (Add v1 v2))) Nil (Some k)) = do
  x <- fresh "x"
  k' <- t2t (k (VAR x))
  return (add v1 v2 x k')
t2t (Node (R (R (App v vs))) Nil None) =
  return (app v vs)
t2t (Node (R (L (Fix fxs))) bs (Some k)) = do
  bs' <- mapM (\ b -> t2t (b ())) bs
  k' <- t2t (k ())
  return (fix' fxs bs' k')
t2t (Node (L (SetK x v)) Nil (Some k)) =
  local ((x,v):) (t2t (k ()))
t2t (Node (L (GetK x)) Nil (Some k)) = do
  nv <- ask
  case lookup x nv of
    Just v -> t2t (k v)
    Nothing -> error (x ++ " is not in env " ++ show nv)
t2t (Node (L Block) (b ::: Nil) (Some k)) = do
  r <- fresh "r"
  x <- fresh "x"
  b' <- local (("_nxt",LABEL r):) (t2t (do v <- b (); T.app (LABEL r) [v]))
  k' <- t2t (k (VAR x))
  return (fix' ((r,[x]) ::: Nil) (k' ::: Nil) b')
t2t (Node (L (Fresh x)) Nil (Some k)) = do
  f <- fresh x
  t2t (k f)
