{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Tree2Tps where

import Val
import Vec
import Option

import Tree (Tree(Leaf,Node),Cmd)
import qualified Tree as T
import Tps hiding (Leaf,Node)

import Control.Monad.State hiding (fix)
import Control.Monad.Reader hiding (fix)

type M = StateT Int (Reader [(String,Val)])

tree2tps :: Tree Cmd Val -> Tps (Fix :+: Base :+: VoidCmd) Val
tree2tps =
  fst .
  flip runReader [] .
  flip runStateT 0 .
  t2t

fresh :: String -> M String
fresh s = do
  i <- get
  put (i+1)
  return $ "_" ++ s ++ show i

t2t :: Tree Cmd Val -> M (Tps (Fix :+: Base :+: VoidCmd) Val)
t2t (Leaf x) = return (done x)
t2t (Node (T.R (T.R (T.Add v1 v2))) Nil (Some k)) = do
  x <- fresh "x"
  k' <- t2t (k (VAR x))
  return (add v1 v2 x k')
t2t (Node (T.R (T.R (T.App v vs))) Nil None) =
  return (app v vs)
t2t (Node (T.R (T.L (T.Fix fxs))) bs (Some k)) = do
  bs' <- mapM (\ b -> t2t (b ())) bs
  k' <- t2t (k ())
  return (fix' fxs bs' k')
t2t (Node (T.L (T.SetK x v)) Nil (Some k)) =
  local ((x,v):) (t2t (k ()))
t2t (Node (T.L (T.GetK x)) Nil (Some k)) = do
  nv <- ask
  case lookup x nv of
    Just v -> t2t (k v)
    Nothing -> error (x ++ " is not in env " ++ show nv)
t2t (Node (T.L T.Block) (b ::: Nil) (Some k)) = do
  r <- fresh "r"
  x <- fresh "x"
  b' <- local (("_nxt",LABEL r):) (t2t (do v <- b (); T.app (LABEL r) [v]))
  k' <- t2t (k (VAR x))
  return (fix' ((r,[x]) ::: Nil) (k' ::: Nil) b')
t2t (Node (T.L (T.Fresh x)) Nil (Some k)) = do
  f <- fresh x
  t2t (k f)
