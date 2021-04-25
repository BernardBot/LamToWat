{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Trans.Tree2Tps where

import Val
import Types (fresh)
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

tree2tps :: LamTree -> LamTps
tree2tps =
    fst .
    flip runReader [] .
    flip runStateT 0 .
    tree2tps'

tree2tps' :: LamTree -> TransM LamTps
tree2tps' (Leaf x) = return (done x)
tree2tps' (Node (R (R (Add v1 v2))) Nil (Some k)) = do
  x <- fresh "x"
  k' <- tree2tps' (k (VAR x))
  return (add v1 v2 x k')
tree2tps' (Node (R (R (App v vs))) Nil None) =
  return (app v vs)
tree2tps' (Node (R (L (Fix fxs))) bs (Some k)) = do
  bs' <- mapM (\ b -> tree2tps' (b ())) bs
  k' <- tree2tps' (k ())
  return (fix' fxs bs' k')
tree2tps' (Node (L (SetK x v)) Nil (Some k)) =
  local ((x,v):) (tree2tps' (k ()))
tree2tps' (Node (L (GetK x)) Nil (Some k)) = do
  nv <- ask
  case lookup x nv of
    Just v -> tree2tps' (k v)
    Nothing -> error (x ++ " is not in env " ++ show nv)
tree2tps' (Node (L Block) (b ::: Nil) (Some k)) = do
  r <- fresh "r"
  x <- fresh "x"
  b' <- local
    (("_nxt",LABEL r):)
    (tree2tps' (do v <- b ()
                   T.app (LABEL r) [v]))
  k' <- tree2tps' (k (VAR x))
  return (fix' ((r,[x]) ::: Nil) (k' ::: Nil) b')
tree2tps' (Node (L (Fresh x)) Nil (Some k)) = do
  f <- fresh x
  tree2tps' (k f)
