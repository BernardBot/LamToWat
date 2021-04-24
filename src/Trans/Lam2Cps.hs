{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trans.Lam2Cps where

import Control.Monad.Cont
import Control.Monad.State

import Types

import Lam.Syntax
import Cps.Syntax

type TransM = ContT Cps (State Int)

instance Transformable Lam Cps where
  transform =
    fst .
    flip runState 0 .
    flip runContT (return . DONE) .
    trans

trans :: Lam -> TransM Val
trans (Val v) = return v
trans (Lam x e) = ContT (\ c -> do
  f <- fresh "f"
  k <- fresh "k"
  cf <- runContT (trans e) (\ z ->
          return (APP (VAR k) [z]))
  c' <- c (LABEL f)
  return (FIX [(f,[x,k],cf)] c'))
trans (App e1 e2) = do
  v1 <- trans e1
  v2 <- trans e2
  ContT (\ c -> do
    r <- fresh "r"
    x <- fresh "x"
    c' <- c (VAR x)
    return (FIX [(r,[x],c')] (APP v1 [v2, LABEL r])))
trans (Add e1 e2) = do
  v1 <- trans e1
  v2 <- trans e2
  ContT (\ c -> do
    x <- fresh "x"
    c' <- c (VAR x)
    return (ADD v1 v2 x c'))
