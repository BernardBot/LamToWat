module Cont.Lam2Cps where

import Control.Monad.Cont
import Control.Monad.State

import Val
import Types (fresh)

import Lam
import Cont.Cps

type TransM = ContT Cps (State Int)

lam2cps :: Lam -> Cps
lam2cps =
    fst .
    flip runState 0 .
    flip runContT (return . DONE) .
    l2c

l2c :: Lam -> TransM Val
l2c (Val v) = return v
l2c (Lam x e) = ContT (\ c -> do
  f <- fresh "f"
  k <- fresh "k"
  cf <- runContT (l2c e) (\ z ->
          return (APP (VAR k) [z]))
  c' <- c (LABEL f)
  return (FIX [(f,[x,k],cf)] c'))
l2c (App e1 e2) = do
  v1 <- l2c e1
  v2 <- l2c e2
  ContT (\ c -> do
    r <- fresh "r"
    x <- fresh "x"
    c' <- c (VAR x)
    return (FIX [(r,[x],c')] (APP v1 [v2, LABEL r])))
l2c (Add e1 e2) = do
  v1 <- l2c e1
  v2 <- l2c e2
  ContT (\ c -> do
    x <- fresh "x"
    c' <- c (VAR x)
    return (ADD v1 v2 x c'))
