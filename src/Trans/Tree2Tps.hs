{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Trans.Tree2Tps where

import Types (Val(INT,VAR,LABEL),fresh,Transformable,transform)
import Vec
import Option
import Commands
import Union

import Tree.Syntax
import qualified Tree.Commands as T (app)

import Tps.Syntax (Tps)
import Tps.Commands

import Control.Monad.State (StateT,runStateT)
import Control.Monad.Reader (Reader,runReader,ask,local)

type LamTree = Tree (Comp :+: Fix :+: Base) Val
type LamTps = Tps (Fix :+: Base :+: Empty) Val
type TransM = StateT Int (Reader [(String,Val)])

instance Transformable LamTree LamTps where
  transform = 
    fst .
    flip runReader [] .
    flip runStateT 0 .
    trans

trans :: LamTree -> TransM LamTps
trans (Leaf x) = return (done x)
trans (Node (R (R (Add v1 v2))) Nil (Some k)) = do
  x <- fresh "x"
  k' <- trans (k (VAR x))
  return (add v1 v2 x k')
trans (Node (R (R (App v vs))) Nil None) =
  return (app v vs)
trans (Node (R (L (Fix fxs))) bs (Some k)) = do
  bs' <- mapM (\ b -> trans (b ())) bs
  k' <- trans (k ())
  return (fix' fxs bs' k')
trans (Node (L (SetK x v)) Nil (Some k)) =
  local ((x,v):) (trans (k ()))
trans (Node (L (GetK x)) Nil (Some k)) = do
  nv <- ask
  case lookup x nv of
    Just v -> trans (k v)
    Nothing -> error (x ++ " is not in env " ++ show nv)
trans (Node (L Block) (b ::: Nil) (Some k)) = do
  r <- fresh "r"
  x <- fresh "x"
  b' <- local
    (("_nxt",LABEL r):)
    (trans (do v <- b ()
               T.app (LABEL r) [v]))
  k' <- trans (k (VAR x))
  return (fix' ((r,[x]) ::: Nil) (k' ::: Nil) b')
trans (Node (L (Fresh x)) Nil (Some k)) = do
  f <- fresh x
  trans (k f)
