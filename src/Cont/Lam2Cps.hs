module Cont.Lam2Cps where

import Control.Monad.Cont
import Control.Monad.State

import Val
import Types (fresh)

import Lam
import Cont.Cps

lam2cps :: Lam -> Cps
lam2cps =
    fst .
    flip runState 0 .
    flip l2c (return . DONE)

l2c :: Lam -> (Val -> State Int Cps) -> State Int Cps
l2c (Val v) c = c v
l2c (Lam x e) c = do
  f <- fresh "f"
  k <- fresh "k"
  c' <- c (LABEL f)
  cf <- l2c e $ \ z -> return $ APP (VAR k) [z]
  return $ FIX [(f,[x,k],cf)] c'
l2c (App e1 e2) c = do
  r <- fresh "r"
  x <- fresh "x"
  c' <- c (VAR x)
  cf <- l2c e1 $ \ v1 -> l2c e2 $ \ v2 -> return $ APP v1 [v2, LABEL r]
  return $ FIX [(r,[x],c')] cf
l2c (Add e1 e2) c = do
  x <- fresh "x"
  c' <- c (VAR x)
  l2c e1 $ \ v1 -> l2c e2 $ \ v2 -> return $ ADD v1 v2 x c'
