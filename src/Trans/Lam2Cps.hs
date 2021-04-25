module Trans.Lam2Cps where

import Control.Monad.Cont
import Control.Monad.State

import Val
import Types (fresh)

import Lam.Syntax
import Cps.Syntax

type TransM = ContT Cps (State Int)

lam2cps :: Lam -> Cps
lam2cps =
    fst .
    flip runState 0 .
    flip runContT (return . DONE) .
    lam2cps'

lam2cps' :: Lam -> TransM Val
lam2cps' (Val v) = return v
lam2cps' (Lam x e) = ContT (\ c -> do
  f <- fresh "f"
  k <- fresh "k"
  cf <- runContT (lam2cps' e) (\ z ->
          return (APP (VAR k) [z]))
  c' <- c (LABEL f)
  return (FIX [(f,[x,k],cf)] c'))
lam2cps' (App e1 e2) = do
  v1 <- lam2cps' e1
  v2 <- lam2cps' e2
  ContT (\ c -> do
    r <- fresh "r"
    x <- fresh "x"
    c' <- c (VAR x)
    return (FIX [(r,[x],c')] (APP v1 [v2, LABEL r])))
lam2cps' (Add e1 e2) = do
  v1 <- lam2cps' e1
  v2 <- lam2cps' e2
  ContT (\ c -> do
    x <- fresh "x"
    c' <- c (VAR x)
    return (ADD v1 v2 x c'))
